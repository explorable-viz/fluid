module Desugar where

import Prelude hiding (absurd)
import Data.Foldable (foldM)
import Data.List (List(..), (:), (\\), head, length)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map)
import Data.Map (fromFoldable, singleton, toUnfoldable) as M
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import DataType (Ctr, DataType'(..), arity, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), VarDef(..), Var)
import Expr (Expr(..), RecDefs, RawExpr(..), expr) as E
import Lattice (𝔹, class BoundedJoinSemilattice, bot)
import Util (MayFail, type (×), (×), (=<<<), (≞), absurd, error, fromJust, mustLookup, report, with)

data RawExpr a =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List (Expr a)) |
   Lambda (Elim a) |
   Lambda2 (NonEmptyList (Branch a)) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   MatchAs (Expr a) (Elim a) |
   IfElse (Expr a) (Expr a) (Expr a) |
   ListSeq (Expr a) (Expr a) |
   ListComp (Expr a) (List (Predicate a)) |
   Let (VarDef a) (Expr a) |
   LetRec (E.RecDefs a) (Expr a)

data Pattern =
   PVar Var |
   PConstr Ctr (List Pattern)

type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

data Predicate a =
   Guard (Expr a) |
   Generator Pattern (Expr a) |
   Declaration Pattern (Expr a)

data Expr a =
   Expr a (RawExpr a)

expr :: forall a . BoundedJoinSemilattice a => RawExpr a -> Expr a
expr = Expr bot

eapp :: E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
eapp f = E.expr <<< E.App f

enil :: E.Expr 𝔹
enil = E.expr $ E.Constr cNil Nil

evar :: Var -> E.Expr 𝔹
evar = E.expr <<< E.Var

desugar :: Expr 𝔹 -> MayFail (E.Expr 𝔹)
desugar (Expr α (Int n))               = pure $ E.Expr α (E.Int n)
desugar (Expr α (IfElse s1 s2 s3))     = do
   e2 <- desugar s2
   e3 <- desugar s3
   let σ = ElimConstr (M.fromFoldable [cTrue × Body e2, cFalse × Body e3])
   E.Expr α <$> (E.MatchAs <$> desugar s1 <@> σ)
desugar (Expr α (ListSeq s1 s2))       =
   eapp <$> (eapp (evar "range") <$> desugar s1) <*> desugar s2
desugar (Expr α (ListComp s_body (Guard (Expr _ (Constr cTrue Nil)) : Nil))) = do
   e <- desugar s_body
   pure $ E.expr $ E.Constr cCons (e : enil : Nil)
desugar (Expr α (ListComp s_body (q:Nil))) =
   desugar $ expr $ ListComp s_body $ q : Guard (expr $ Constr cTrue Nil) : Nil
desugar (Expr α (ListComp s_body (Guard s : qs))) = do
   e <- desugar $ Expr α $ ListComp s_body qs
   let σ = ElimConstr (M.fromFoldable [cTrue × Body e, cFalse × Body enil])
   E.expr <$> (E.MatchAs <$> desugar s <@> σ)
desugar (Expr α (ListComp s_body (Generator p slist : qs))) = do
   e <- desugar $ expr $ ListComp s_body qs
   let λ = E.expr $ E.Lambda $ totalise (patternToElim p (Body e)) enil
   eapp (evar "concat") <$> (eapp (eapp (evar "map") λ) <$> desugar slist)
desugar (Expr α (ListComp s_body (Declaration p s : qs))) = do
   let σ = patternToElim p None
   E.expr <$> (E.Let <$> (VarDef σ <$> desugar s) <*> desugar (Expr α $ ListComp s_body qs))
desugar (Expr α (ListComp s_body _))  = error absurd
desugar (Expr α (Var x))              = pure $ E.Expr α (E.Var x)
desugar (Expr α (Op op))              = pure $ E.Expr α (E.Op op)
desugar (Expr α (Str s))              = pure $ E.Expr α (E.Str s)
desugar (Expr α (Constr ctr args))    = E.Expr α <$> (E.Constr ctr <$> traverse desugar args)
desugar (Expr α (Lambda σ))           = pure $ E.Expr α (E.Lambda σ)
desugar (Expr α (Lambda2 bs))         = E.Expr α <$> (E.Lambda <$> joinAll bs)
desugar (Expr α (App s1 s2))          = E.Expr α <$> (E.App <$> desugar s1 <*> desugar s2)
desugar (Expr α (BinaryApp s1 op s2)) = E.Expr α <$> (E.BinaryApp <$> desugar s1 <@> op <*> desugar s2)
desugar (Expr α (MatchAs s σ))        = E.Expr α <$> (E.MatchAs <$> desugar s <@> σ)
desugar (Expr α (Let def s))          = E.Expr α <$> (E.Let def <$> desugar s)
desugar (Expr α (LetRec δ s))         = E.Expr α <$> (E.LetRec δ <$> desugar s)

patternToElim :: Pattern -> Cont 𝔹 -> Elim 𝔹
patternToElim (PVar x) κ
   = ElimVar x κ
patternToElim (PConstr ctr ps) κ
   = let go (p':p'':ps')   = Arg (patternToElim p' (go (p'':ps')))
         go (p':Nil)       = Arg (patternToElim p' κ)
         go Nil            = κ
     in  ElimConstr (M.singleton ctr (go ps))

totalise :: Elim 𝔹 -> E.Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e
   = let ctr × κ              = fromJust "" (head $ M.toUnfoldable m)
         branches             = M.toUnfoldable m
         DataType _ sigs      = mustLookup ctr ctrToDataType
         all_ctrs             = fst <$> M.toUnfoldable sigs
         new_branches         = (_ × Body e) <$> (all_ctrs \\ (fst <$> branches))
         totalised_branches   = branches <#>
                                 \(c × κ) -> case mustLookup c m of
                                                Arg σ   -> c × Arg (totalise σ e)
                                                Body e' -> c × Body e'
                                                None    -> c × Body e
     in   ElimConstr (M.fromFoldable $ totalised_branches <> new_branches)
totalise (ElimVar e k) e'
   = case k of Arg σ  -> ElimVar e $ Arg (totalise σ e')
               Body _ -> ElimVar e k
               None   -> ElimVar e $ Body e'

toCont2 :: List Pattern -> Cont 𝔹 -> MayFail (Cont 𝔹)
toCont2 Nil κ        = pure κ
toCont2 (π : πs) κ   = do
   κ' <- toCont2 πs κ
   Arg <$> toElim2 π κ'

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = void $ with ("Checking arity of " <> show c) $
   arity c `(=<<<) (≞)` pure n

toElim2 :: Pattern -> Cont 𝔹 -> MayFail (Elim 𝔹)
toElim2 (PVar x) κ       = pure $ ElimVar x κ
toElim2 (PConstr c πs) κ = checkArity c (length πs) *> (ElimConstr <$> M.singleton c <$> toCont2 πs κ)

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
   maybeJoin m1 m2 = do
      let kvs = M.toUnfoldable m2 :: List (Ctr × Cont 𝔹)
      error "todo"

joinAll :: NonEmptyList (Branch 𝔹) -> MayFail (Elim 𝔹)
joinAll bs = do
   NonEmptyList (σ :| σs) <- traverse (\(πs × e) -> (Body <$> desugar e) >>= toElim πs) bs
   foldM maybeJoin σ σs
