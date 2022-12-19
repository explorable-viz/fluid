module EvalFwd where

import Prelude hiding (absurd, top)

import Bindings (varAnon)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), length, range, singleton)
import Data.Profunctor.Strong ((***), first, second)
import Data.Set (toUnfoldable) as S
import Data.Set (union)
import Dict (disjointUnion, empty, get)
import Dict (singleton) as O
import Expr (Cont, Elim(..), Expr(..), RecDefs, VarDef(..), asElim, asExpr, fv)
import Lattice (class BoundedMeetSemilattice, (∧), top)
import Pretty (class Highlightable)
import Primitive (unwrap)
import Util (type (×), (×), absurd, error)
import Val (Env, PrimOp(..), (<+>), Val, for, restrict)
import Val (Val(..)) as V

matchFwd :: forall a. BoundedMeetSemilattice a => Val a -> Elim a -> Env a × Cont a × a
matchFwd v (ElimVar x κ)
   | x == varAnon = empty × κ × top
   | otherwise = O.singleton x v × κ × top
matchFwd (V.Constr α c vs) (ElimConstr m) =
   second (_ ∧ α) (matchManyFwd vs (get c m))
matchFwd (V.Record α xvs) (ElimRecord xs κ) =
   second (_ ∧ α) (matchManyFwd (xs # S.toUnfoldable <#> flip get xvs) κ)
matchFwd _ _ = error absurd

matchManyFwd :: forall a. BoundedMeetSemilattice a => List (Val a) -> Cont a -> Env a × Cont a × a
matchManyFwd Nil κ = empty × κ × top
matchManyFwd (v : vs) σ =
   (first (ρ `disjointUnion` _) *** (_ ∧ α)) (matchManyFwd vs κ)
   where
   ρ × κ × α = matchFwd v (asElim σ)

closeDefsFwd :: forall a. Env a -> RecDefs a -> a -> Env a
closeDefsFwd γ ρ α = ρ <#> \σ ->
   let ρ' = ρ `for` σ in V.Closure α (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

evalFwd :: forall a. BoundedMeetSemilattice a => Highlightable a => Env a -> Expr a -> a -> Val a
evalFwd γ (Var x) _ = get x γ
evalFwd γ (Op op) _ = get op γ
evalFwd _ (Int α n) α' = V.Int (α ∧ α') n
evalFwd _ (Float α n) α' = V.Float (α ∧ α') n
evalFwd _ (Str α s) α' = V.Str (α ∧ α') s
evalFwd γ (Record α xes) α' =
   V.Record (α ∧ α') xvs
   where
   xvs = xes <#> (\e -> evalFwd γ e α')
evalFwd γ (Constr α c es) α' =
   V.Constr (α ∧ α') c ((\e' -> evalFwd γ e' α') <$> es)
-- here
evalFwd γ (Matrix α e1 (x × y) e2) α' =
   let
      (i' × β) × (j' × β') = unwrap $ evalFwd γ e2 α'
      vss = A.fromFoldable $ do
         i <- range 1 i'
         singleton $ A.fromFoldable $ do
            j <- range 1 j'
            let γ' = O.singleton x (V.Int β i) `disjointUnion` (O.singleton y (V.Int β' j))
            singleton (evalFwd (γ <+> γ') e1 α')
   in
      V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
evalFwd γ (Lambda σ) α = V.Closure α (γ `restrict` fv σ) empty σ
evalFwd γ (Project e' x) α =
   case evalFwd γ e' α of
      V.Record _ xvs -> get x xvs
      _ -> error absurd
evalFwd γ (App e1 e2) α =
   case evalFwd γ e1 α of
      V.Closure β γ1 δ σ' ->
         evalFwd (γ1 <+> γ2 <+> γ3) (asExpr e3) (β ∧ β')
         where
         v = evalFwd γ e2 α
         γ2 = closeDefsFwd γ1 δ β
         γ3 × e3 × β' = matchFwd v σ'
      V.Primitive (PrimOp φ) vs' ->
         if φ.arity > length vs'' then V.Primitive (PrimOp φ) vs'' else φ.op vs''
         where
         v2' = evalFwd γ e2 α
         vs'' = vs' <> singleton v2'
      V.Constr α' c vs' ->
         V.Constr (α ∧ α') c (vs' <> singleton v)
         where
         v = evalFwd γ e2 α
      _ -> error absurd
evalFwd γ (Let (VarDef σ e1) e2) α =
   evalFwd (γ <+> γ') e2 α'
   where
   v = evalFwd γ e1 α
   γ' × _ × α' = matchFwd v σ
evalFwd γ (LetRec ρ e') α =
   evalFwd (γ <+> γ') e' α
   where
   γ' = closeDefsFwd γ ρ α
evalFwd _ _ _ = error absurd
