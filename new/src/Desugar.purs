module Desugar where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (on)
import Data.List (List(..), (:), (\\), length)
import Data.List (head) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, reverse, toList)
import Data.Map (Map, fromFoldable, insert, lookup, singleton, toUnfoldable, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr, DataType, DataType'(..), arity, ctrToDataType, cCons, cNil, cTrue, cFalse, dataTypeFor)
import Expr (Cont(..), Elim(..), Var)
import Expr (Expr(..), Module(..), RawExpr(..), VarDef(..), expr) as E
import Lattice (𝔹, class BoundedJoinSemilattice, bot)
import Util (
   MayFail, type (×), (×), type (+), (=<<<), (≞), absurd, error, fromJust, mustLookup, report, successfulWith, with
)

data RawExpr a =
   Var Var |
   Op Var |
   Int Int |
   Float Number |
   Str String |
   Constr Ctr (List (Expr a)) |
   Lambda (NonEmptyList (Branch a)) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   MatchAs (Expr a) (NonEmptyList (Branch a)) |
   IfElse (Expr a) (Expr a) (Expr a) |
   ListRange (Expr a) (Expr a) |
   ListComp (Expr a) (List (Qualifier a)) |
   Let (VarDef a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

data Pattern =
   PVar Var |
   PConstr Ctr (List Pattern)

type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)
type VarDef a = Pattern × Expr a
type VarDefs a = List (VarDef a)

data Qualifier a =
   Guard (Expr a) |
   Generator Pattern (Expr a) |
   Declaration Pattern (Expr a)

data Expr a =
   Expr a (RawExpr a)

data Module a = Module (List (VarDef a + RecDefs a))

expr :: forall a . BoundedJoinSemilattice a => RawExpr a -> Expr a
expr = Expr bot

eapp :: E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
eapp f = E.expr <<< E.App f

enil :: E.Expr 𝔹
enil = E.expr $ E.Constr cNil Nil

evar :: Var -> E.Expr 𝔹
evar = E.expr <<< E.Var

class Desugarable a b where
   desugar :: a -> MayFail b

instance desugarVarDef :: Desugarable (Tuple Pattern (Expr Boolean)) (E.VarDef Boolean) where
   desugar (p × s) = E.VarDef (patternToElim p None) <$> desugar s

instance desugarRecDefs :: Desugarable (NonEmptyList (Tuple String (Tuple (NonEmptyList Pattern) (Expr Boolean))))
                                       (Bindings Elim Boolean) where
   desugar fπs = pure δ
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))
      δ = fromList $ toList $ reverse $ toRecDef <$> fπss

      toRecDef :: NonEmptyList (Clause 𝔹) -> Binding Elim 𝔹
      toRecDef fπs' =
         let f = fst $ head fπs' in
         f ↦ successfulWith ("Bad branches for '" <> f <> "'") (joinAll $ snd <$> fπs')

instance desugarExpr :: Desugarable (Expr Boolean) (E.Expr Boolean) where
   desugar (Expr α (Int n))               = pure $ E.Expr α (E.Int n)
   desugar (Expr α (Float n))             = pure $ E.Expr α (E.Float n)
   desugar (Expr α (Var x))               = pure $ E.Expr α (E.Var x)
   desugar (Expr α (Op op))               = pure $ E.Expr α (E.Op op)
   desugar (Expr α (Str s))               = pure $ E.Expr α (E.Str s)
   desugar (Expr α (Constr ctr args))     = E.Expr α <$> (E.Constr ctr <$> traverse desugar args)
   desugar (Expr α (Lambda bs))           = E.Expr α <$> (E.Lambda <$> joinAll bs)
   desugar (Expr α (App s1 s2))           = E.Expr α <$> (E.App <$> desugar s1 <*> desugar s2)
   desugar (Expr α (BinaryApp s1 op s2))  = E.Expr α <$> (E.BinaryApp <$> desugar s1 <@> op <*> desugar s2)
   desugar (Expr α (MatchAs s bs))        = E.Expr α <$> (E.MatchAs <$> desugar s <*> joinAll bs)
   desugar (Expr α (Let d s'))            = E.Expr α <$> (E.Let <$> desugar d <*> desugar s')
   desugar (Expr α (LetRec fπs s))        = E.Expr α <$> (E.LetRec <$> desugar fπs <*> desugar s)
   desugar (Expr α (IfElse s1 s2 s3)) = do
      e2 <- desugar s2
      e3 <- desugar s3
      let σ = ElimConstr (fromFoldable [cTrue × Body e2, cFalse × Body e3])
      E.Expr α <$> (E.MatchAs <$> desugar s1 <@> σ)
   desugar (Expr α (ListRange s1 s2)) =
      eapp <$> (eapp (evar "range") <$> desugar s1) <*> desugar s2
   desugar (Expr α (ListComp s_body (Guard (Expr _ (Constr cTrue Nil)) : Nil))) = do
      e <- desugar s_body
      pure $ E.expr $ E.Constr cCons (e : enil : Nil)
   desugar (Expr α (ListComp s_body (q:Nil))) =
      desugar $ expr $ ListComp s_body $ q : Guard (expr $ Constr cTrue Nil) : Nil
   desugar (Expr α (ListComp s_body (Guard s : qs))) = do
      e <- desugar $ Expr α $ ListComp s_body qs
      let σ = ElimConstr (fromFoldable [cTrue × Body e, cFalse × Body enil])
      E.expr <$> (E.MatchAs <$> desugar s <@> σ)
   desugar (Expr α (ListComp s_body (Generator p slist : qs))) = do
      e <- desugar $ expr $ ListComp s_body qs
      let λ = E.expr $ E.Lambda $ totalise (patternToElim p (Body e)) enil
      eapp (evar "concat") <$> (eapp (eapp (evar "map") λ) <$> desugar slist)
   desugar (Expr α (ListComp s_body (Declaration p s : qs))) = do
      let σ = patternToElim p None
      E.expr <$> (E.Let <$> (E.VarDef σ <$> desugar s) <*> desugar (Expr α $ ListComp s_body qs))
   desugar (Expr _ (ListComp _ Nil)) = error absurd

instance desugarModule :: Desugarable (Module Boolean) (E.Module Boolean) where
   desugar (Module Nil) = pure $ E.Module Nil
   desugar (Module (Left d : ds)) = do
      E.Module ds' <- desugar $ Module ds
      d' <- desugar d
      pure $ E.Module $ Left d' : ds'
   desugar (Module (Right fπs : ds)) = do
      E.Module ds' <- desugar $ Module ds
      δ <- desugar fπs
      pure $ E.Module $ Right δ : ds'

patternToElim :: Pattern -> Cont 𝔹 -> Elim 𝔹
patternToElim (PVar x) κ
   = ElimVar x κ
patternToElim (PConstr ctr ps) κ
   = let go (p':p'':ps')   = Arg (patternToElim p' (go (p'':ps')))
         go (p':Nil)       = Arg (patternToElim p' κ)
         go Nil            = κ
     in  ElimConstr (singleton ctr (go ps))

totalise :: Elim 𝔹 -> E.Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e
   = let ctr × κ              = fromJust "" (L.head $ toUnfoldable m)
         branches             = toUnfoldable m
         DataType _ sigs      = mustLookup ctr ctrToDataType
         all_ctrs             = fst <$> toUnfoldable sigs
         new_branches         = (_ × Body e) <$> (all_ctrs \\ (fst <$> branches))
         totalised_branches   = branches <#>
                                 \(c × κ) -> case mustLookup c m of
                                                Arg σ   -> c × Arg (totalise σ e)
                                                Body e' -> c × Body e'
                                                None    -> c × Body e
     in   ElimConstr (fromFoldable $ totalised_branches <> new_branches)
totalise (ElimVar e k) e'
   = case k of Arg σ  -> ElimVar e $ Arg (totalise σ e')
               Body _ -> ElimVar e k
               None   -> ElimVar e $ Body e'

toCont2 :: List Pattern -> Cont 𝔹 -> MayFail (Cont 𝔹)
toCont2 Nil κ        = pure κ
toCont2 (π : πs) κ   = Arg <$> (toCont2 πs κ >>= toElim2 π)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = void $ with ("Checking arity of " <> show c) $
   arity c `(=<<<) (≞)` pure n

dataType :: Map Ctr (Cont 𝔹) -> MayFail DataType
dataType κs = case keys κs of
   Nil   -> error absurd
   c : _ -> dataTypeFor c

checkDataType :: String -> Ctr -> Map Ctr (Cont 𝔹) -> MayFail Unit
checkDataType msg c κs = void $ do
   d <- dataTypeFor c
   d' <- dataType κs
   if (d /= d')
   then error "***"
   else with (msg <> show c <> " is not a constructor of " <> show d') $ d ≞ d'

toElim2 :: Pattern -> Cont 𝔹 -> MayFail (Elim 𝔹)
toElim2 (PVar x) κ       = pure $ ElimVar x κ
toElim2 (PConstr c πs) κ = checkArity c (length πs) *> (ElimConstr <$> singleton c <$> toCont2 πs κ)

toElim :: NonEmptyList Pattern -> Cont 𝔹 -> MayFail (Elim 𝔹)
toElim (NonEmptyList (π :| Nil)) κ     = toElim2 π κ
toElim (NonEmptyList (π :| π' : πs)) κ =
   toElim2 π =<< Body <$> E.expr <$> E.Lambda <$> toElim (NonEmptyList $ π' :| πs) κ

class Joinable a where
   maybeJoin :: a -> a -> MayFail a

instance joinableElim :: Joinable (Elim Boolean) where
   maybeJoin (ElimVar x κ) (ElimVar y κ')       = ElimVar <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = report "Can't join variable and constructor patterns"

instance joinableCont :: Joinable (Cont Boolean) where
   maybeJoin None None                       = pure None
   maybeJoin (Arg σ) (Arg σ')                = Arg <$> maybeJoin σ σ'
   maybeJoin (Body (E.Expr _ (E.Lambda σ)))
             (Body (E.Expr _ (E.Lambda σ'))) = Body<$> (E.expr <$> (E.Lambda <$> maybeJoin σ σ'))
   maybeJoin _ _                             = report "Incompatible continuations"

instance joinableMap :: Joinable (Map Ctr (Cont Boolean)) where
   maybeJoin κs1 κs2 = do
      foldM maybeUpdate κs1 (toUnfoldable κs2 :: List (Ctr × Cont 𝔹))
      where
      maybeUpdate :: Map Ctr (Cont 𝔹) -> Ctr × Cont 𝔹 -> MayFail (Map Ctr (Cont 𝔹))
      maybeUpdate κs (c × κ) =
         case lookup c κs of
            Nothing -> do
               checkDataType "Non-uniform patterns: " c κs
               pure $ insert c κ κs
            Just κ' ->
               update <$> (const <$> pure <$> maybeJoin κ' κ) <@> c <@> κs

joinAll :: NonEmptyList (Branch 𝔹) -> MayFail (Elim 𝔹)
joinAll bs = do
   NonEmptyList (σ :| σs) <- traverse (\(πs × e) -> (Body <$> desugar e) >>= toElim πs) bs
   foldM maybeJoin σ σs
