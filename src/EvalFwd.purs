module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), length, range, singleton, zip)
import Data.Profunctor.Strong ((***), first, second)
import Data.Set (union)
import Data.Set (toUnfoldable) as S
import Bindings (varAnon)
import Dict (disjointUnion, empty, get, intersectionWith)
import Dict (singleton) as O
import Expr (Cont, Elim(..), Expr(..), RecDefs, VarDef(..), asElim, asExpr, fv)
import Lattice (𝔹, (∧))
import Primitive (match) as P
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace)
import Util (type (×), (×), (!), absurd, assert, error)
import Val (Env, PrimOp(..), (<+>), Val, for, restrict)
import Val (Val(..)) as V

matchFwd :: Val 𝔹 -> Elim 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchFwd v (ElimVar x κ)
   | x == varAnon = empty × κ × true
   | otherwise = O.singleton x v × κ × true
matchFwd (V.Constr α c vs) (ElimConstr m)  =
   second (_ ∧ α) (matchManyFwd vs (get c m))
matchFwd (V.Record α xvs) (ElimRecord xs κ) =
   second (_ ∧ α) (matchManyFwd (xs # S.toUnfoldable <#> flip get xvs) κ)
matchFwd _ _ = error absurd

matchManyFwd :: List (Val 𝔹) -> Cont 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchManyFwd Nil κ = empty × κ × true
matchManyFwd (v : vs) σ =
   (first (ρ `disjointUnion` _) *** (_ ∧ α)) (matchManyFwd vs κ)
   where
   ρ × κ × α = matchFwd v (asElim σ)

closeDefsFwd :: Env 𝔹 -> RecDefs 𝔹 -> 𝔹 -> Env 𝔹
closeDefsFwd γ ρ α = ρ <#> \σ ->
   let ρ' = ρ `for` σ in V.Closure α (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

evalFwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Trace 𝔹 -> Val 𝔹
evalFwd γ (Var x) _ (T.Var _) = get x γ
evalFwd γ (Op op) _ (T.Op _) = get op γ
evalFwd _ (Int α n) α' (T.Int _) = V.Int (α ∧ α') n
evalFwd _ (Float α n) α' (T.Float _) = V.Float (α ∧ α') n
evalFwd _ (Str α s) α' (T.Str _) = V.Str (α ∧ α') s
evalFwd γ (Record α xes) α' (T.Record xts) =
   V.Record (α ∧ α') xvs
   where
   xvs = intersectionWith (×) xes xts <#> (\(e × t) -> evalFwd γ e α' t)
evalFwd γ (Constr α _ es) α' (T.Constr c ts) =
   V.Constr (α ∧ α') c ((\(e' × t) -> evalFwd γ e' α' t) <$> zip es ts)
evalFwd γ (Matrix α e1 _ e2) α' (T.Matrix tss (x × y) (i' × j') t2) =
   case evalFwd γ e2 α' t2 of
      V.Constr _ _ (v1 : v2 : Nil) ->
         V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
         where
         (i'' × β) × (j'' × β') = P.match v1 × P.match v2
         vss = assert (i'' == i' && j'' == j') $ A.fromFoldable $ do
            i <- range 1 i'
            singleton $ A.fromFoldable $ do
               j <- range 1 j'
               let γ' = O.singleton x (V.Int β i) `disjointUnion` (O.singleton y (V.Int β' j))
               singleton (evalFwd (γ <+> γ') e1 α' (tss ! (i - 1) ! (j - 1)))
      _ -> error absurd
evalFwd γ (Lambda σ) α (T.Lambda _) = V.Closure α (γ `restrict` fv σ) empty σ
evalFwd γ (Project e' x) α (T.Project t _) =
   case evalFwd γ e' α t of
      V.Record _ xvs -> get x xvs
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.App (t1 × _ × _) t2 _ t3) =
   case evalFwd γ e1 α t1 of
      V.Closure β γ1 δ σ' ->
         evalFwd (γ1 <+> γ2 <+> γ3) (asExpr e3) (β ∧ β') t3
         where
         v = evalFwd γ e2 α t2
         γ2 = closeDefsFwd γ1 δ β
         γ3 × e3 × β' = matchFwd v σ'
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppPrim (t1 × PrimOp φ × _) (t2 × _)) =
   case evalFwd γ e1 α t1 of
      V.Primitive _ vs' ->
         if φ.arity > length vs'' then V.Primitive (PrimOp φ) vs'' else φ.op vs''
         where
         v2' = evalFwd γ e2 α t2
         vs'' = vs' <> singleton v2'
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppConstr (t1 × c × _) t2) =
   case evalFwd γ e1 α t1 of
      V.Constr α' _ vs' ->
         V.Constr (α ∧ α') c (vs' <> singleton v)
         where
         v = evalFwd γ e2 α t2
      _ -> error absurd
evalFwd γ (Let (VarDef σ e1) e2) α (T.Let (T.VarDef _ t1) t2) =
   evalFwd (γ <+> γ') e2 α' t2
   where
   v = evalFwd γ e1 α t1
   γ' × _ × α' = matchFwd v σ
evalFwd γ (LetRec ρ e') α (T.LetRec _ t) =
   evalFwd (γ <+> γ') e' α t
   where
   γ' = closeDefsFwd γ ρ α
evalFwd _ _ _ _ = error absurd
