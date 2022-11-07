module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), length, range, singleton, zip)
import Data.Profunctor.Strong ((***), first, second)
import Data.Set (union)
import Data.Set (toUnfoldable) as S
import Data.Tuple (snd)
import Dict (disjointUnion, empty, get, intersectionWith)
import Dict (singleton, toUnfoldable) as O
import Expr (Cont, Elim(..), Expr(..), RecDefs, VarDef(..), asElim, asExpr, fv)
import Lattice (𝔹, (∧))
import Primitive (match) as P
import Trace (Trace(..), Match(..), VarDef(..)) as T
import Trace (Trace, Match)
import Util (type (×), (×), (!), absurd, assert, error)
import Val (Env, PrimOp(..), (<+>), Val, for, restrict)
import Val (Val(..)) as V

matchFwd :: Val 𝔹 -> Elim 𝔹 -> Match 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchFwd _ (ElimVar _ κ) (T.MatchVarAnon _) = empty × κ × true
matchFwd v (ElimVar _ κ) (T.MatchVar x _) = O.singleton x v × κ × true
matchFwd (V.Constr α _ vs) (ElimConstr m) (T.MatchConstr c ws) =
   second (_ ∧ α) (matchManyFwd vs (get c m) ws)
matchFwd (V.Record α xvs) (ElimRecord xs κ) (T.MatchRecord xws) =
   second (_ ∧ α) (matchManyFwd (xs # S.toUnfoldable <#> flip get xvs) κ (xws # O.toUnfoldable <#> snd))
matchFwd _ _ _ = error absurd

matchManyFwd :: List (Val 𝔹) -> Cont 𝔹 -> List (Match 𝔹) -> Env 𝔹 × Cont 𝔹 × 𝔹
matchManyFwd Nil κ Nil = empty × κ × true
matchManyFwd (v : vs) σ (w : ws) =
   (first (ρ `disjointUnion` _) *** (_ ∧ α)) (matchManyFwd vs κ ws)
   where
   ρ × κ × α = matchFwd v (asElim σ) w
matchManyFwd _ _ _ = error absurd

closeDefsFwd :: Env 𝔹 -> RecDefs 𝔹 -> 𝔹 -> Env 𝔹
closeDefsFwd γ ρ α = ρ <#> \σ ->
   V.Closure α (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ
   where
   ρ' = ρ `for` σ

evalFwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Trace 𝔹 -> Val 𝔹
evalFwd γ (Var _) _ (T.Var x) = get x γ
evalFwd γ (Op _) _ (T.Op op) = get op γ
evalFwd _ (Int α _) α' (T.Int n) = V.Int (α ∧ α') n
evalFwd _ (Float α _) α' (T.Float n) = V.Float (α ∧ α') n
evalFwd _ (Str α _) α' (T.Str str) = V.Str (α ∧ α') str
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
evalFwd γ (Project e' _) α (T.Project t x) =
   case evalFwd γ e' α t of
      V.Record _ xvs -> get x xvs
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.App (t1 × _ × _) t2 w t3) =
   case evalFwd γ e1 α t1 of
      V.Closure β γ1 δ σ' ->
         let
            v = evalFwd γ e2 α t2
            γ2 = closeDefsFwd γ1 δ β
            γ3 × e3 × β' = matchFwd v σ' w
         in
            evalFwd (γ1 <+> γ2 <+> γ3) (asExpr e3) (β ∧ β') t3
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppPrim (t1 × PrimOp φ × _) (t2 × _)) =
   case evalFwd γ e1 α t1 of
      V.Primitive _ vs' ->
         let
            v2' = evalFwd γ e2 α t2
            vs'' = vs' <> singleton v2'
         in
            if φ.arity > length vs'' then V.Primitive (PrimOp φ) vs'' else φ.op vs''
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppConstr (t1 × c × _) t2) =
   case evalFwd γ e1 α t1 of
      V.Constr α' _ vs' ->
         let
            v = evalFwd γ e2 α t2
         in
            V.Constr (α ∧ α') c (vs' <> singleton v)
      _ -> error absurd
evalFwd γ (Let (VarDef σ e1) e2) α (T.Let (T.VarDef w t1) t2) =
   let
      v = evalFwd γ e1 α t1
      γ' × _ × α' = matchFwd v σ w
   in
      evalFwd (γ <+> γ') e2 α' t2
evalFwd γ (LetRec ρ e') α (T.LetRec _ t) =
   let
      γ' = closeDefsFwd γ ρ α
   in
      evalFwd (γ <+> γ') e' α t
evalFwd _ _ _ _ = error absurd
