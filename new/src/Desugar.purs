module Desugar where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (on)
import Data.List (List(..), (:), (\\), length)
import Data.List (head) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, reverse, toList)
import Data.Map (Map, fromFoldable, insert, lookup, singleton, toUnfoldable, update)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr, DataType'(..), checkArity, checkDataType, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Var)
import Expr (Expr(..), Module(..), RawExpr(..), VarDef(..), expr) as E
import Lattice (𝔹, class BoundedJoinSemilattice, bot)
import Util (MayFail, type (×), (×), type (+), (≞), absurd, error, fromJust, mustLookup, report, successfulWith)

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
   desugar (π × s) = E.VarDef <$> desugar (π × (None :: Cont 𝔹)) <*> desugar s

instance desugarRecDefs :: Desugarable (NonEmptyList (Tuple String (Tuple (NonEmptyList Pattern) (Expr Boolean))))
                                       (Bindings Elim Boolean) where
   desugar fπs = pure δ
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))
      δ = fromList $ toList $ reverse $ toRecDef <$> fπss

      toRecDef :: NonEmptyList (Clause 𝔹) -> Binding Elim 𝔹
      toRecDef fπs' =
         let f = fst $ head fπs' in
         f ↦ successfulWith ("Bad branches for '" <> f <> "'") (desugar $ snd <$> fπs')

instance desugarExpr :: Desugarable (Expr Boolean) (E.Expr Boolean) where
   desugar (Expr α (Int n))               = pure $ E.Expr α (E.Int n)
   desugar (Expr α (Float n))             = pure $ E.Expr α (E.Float n)
   desugar (Expr α (Var x))               = pure $ E.Expr α (E.Var x)
   desugar (Expr α (Op op))               = pure $ E.Expr α (E.Op op)
   desugar (Expr α (Str s))               = pure $ E.Expr α (E.Str s)
   desugar (Expr α (Constr ctr args))     = E.Expr α <$> (E.Constr ctr <$> traverse desugar args)
   desugar (Expr α (Lambda bs))           = E.Expr α <$> (E.Lambda <$> desugar bs)
   desugar (Expr α (App s1 s2))           = E.Expr α <$> (E.App <$> desugar s1 <*> desugar s2)
   desugar (Expr α (BinaryApp s1 op s2))  = E.Expr α <$> (E.BinaryApp <$> desugar s1 <@> op <*> desugar s2)
   desugar (Expr α (MatchAs s bs))        = E.Expr α <$> (E.MatchAs <$> desugar s <*> desugar bs)
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
      σ <- desugar $ p × (Body e :: Cont 𝔹)
      let λ = E.expr $ E.Lambda $ totalise σ enil
      eapp (evar "concat") <$> (eapp (eapp (evar "map") λ) <$> desugar slist)
   desugar (Expr α (ListComp s_body (Declaration p s : qs))) = do
      σ <- desugar $ p × (None :: Cont 𝔹)
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

totalise :: Elim 𝔹 -> E.Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e
   = let ctr × κ              = fromJust absurd (L.head $ toUnfoldable m)
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

-- The Cont arguments here act as an accumulator.
instance desugarPattern :: Desugarable (Tuple Pattern (Cont Boolean)) (Elim Boolean) where
   desugar (PVar x × κ)       = pure $ ElimVar x κ
   desugar (PConstr c πs × κ) = checkArity c (length πs) *> (ElimConstr <$> singleton c <$> toCont πs)
      where
      toCont :: List Pattern -> MayFail (Cont 𝔹)
      toCont Nil        = pure κ
      toCont (π : πs')  = Arg <$> do
         κ' <- toCont πs'
         desugar $ π × κ'

instance desugarPatterns :: Desugarable (Tuple (NonEmptyList Pattern) (Cont Boolean)) (Elim Boolean) where
   desugar (NonEmptyList (π :| Nil) × κ)     = desugar $ π × κ
   desugar (NonEmptyList (π :| π' : πs) × κ) = do
      κ' <- Body <$> E.expr <$> E.Lambda <$> desugar (NonEmptyList (π' :| πs) × κ) :: MayFail (Cont 𝔹)
      desugar $ π × κ'

instance desugarBranch :: Desugarable (Tuple (NonEmptyList Pattern) (Expr Boolean)) (Elim Boolean) where
   desugar (πs × s) = do
      κ <- Body <$> desugar s :: MayFail (Cont 𝔹)
      desugar $ πs × κ

instance desugarBranches :: Desugarable (NonEmptyList (NonEmptyList Pattern × Expr Boolean))
                                        (Elim Boolean) where
   desugar bs = do
      NonEmptyList (σ :| σs) <- traverse desugar bs
      foldM maybeJoin σ σs

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
