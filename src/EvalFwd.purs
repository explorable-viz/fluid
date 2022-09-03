module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), length, range, singleton, zip)
import Data.Map (empty, fromFoldable, intersectionWith, keys, toUnfoldable)
import Data.Map (singleton) as M
import Data.Profunctor.Strong ((***), first, second)
import Data.Set (union)
import Bindings (val)
import Expr (Cont, Elim(..), Expr(..), VarDef(..), asElim, asExpr, fv)
import Lattice (𝔹, (∧))
import Primitive (match_fwd) as P
import Trace (Trace(..), Match(..), VarDef(..)) as T
import Trace (Trace, Match)
import Util (type (×), (×), (!), absurd, assert, disjUnion, error, mustLookup, successful)
import Val (Env, FunEnv, PrimOp(..), (<+>), Val, for, lookup', restrict)
import Val (Val(..)) as V

matchFwd :: Val 𝔹 -> Elim 𝔹 -> Match 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchFwd _ (ElimVar _ κ) (T.MatchVarAnon _) = empty × κ × true
matchFwd v (ElimVar _ κ) (T.MatchVar x _) = M.singleton x v × κ × true
matchFwd (V.Constr α _ vs) (ElimConstr m) (T.MatchConstr c ws) =
   second (_ ∧ α) (matchManyFwd vs (mustLookup c m) ws)
matchFwd (V.Record α xvs) (ElimRecord _ κ) (T.MatchRecord xws) =
   second (_ ∧ α) (matchManyFwd (xvs # toUnfoldable <#> val) κ (xws <#> val))
matchFwd _ _ _ = error absurd

matchManyFwd :: List (Val 𝔹) -> Cont 𝔹 -> List (Match 𝔹) -> Env 𝔹 × Cont 𝔹 × 𝔹
matchManyFwd Nil κ Nil = empty × κ × true
matchManyFwd (v : vs) σ (w : ws) =
   let ρ × κ × α = matchFwd v (asElim σ) w in
   (first (ρ `disjUnion` _) *** (_ ∧ α)) (matchManyFwd vs κ ws)
matchManyFwd _ _ _ = error absurd

closeDefsFwd :: Env 𝔹 -> FunEnv 𝔹 -> 𝔹 -> Env 𝔹
closeDefsFwd γ ρ α = ρ <#> \σ ->
   let xs = fv (ρ `for` σ) `union` fv σ
   in V.Closure α (γ `restrict` xs) ρ σ

evalFwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Trace 𝔹 -> Val 𝔹
evalFwd γ (Var _) _ (T.Var x) = successful (lookup' x γ)
evalFwd γ (Op _) _ (T.Op op) = successful (lookup' op γ)
evalFwd _ (Int α _) α' (T.Int n) = V.Int (α ∧ α') n
evalFwd _ (Float α _) α' (T.Float n) = V.Float (α ∧ α') n
evalFwd _ (Str α _) α' (T.Str str) = V.Str (α ∧ α') str
evalFwd γ (Record α xes) α' (T.Record _ xts) =
   let xvs = intersectionWith (×) xes xts <#> (\(e × t) -> evalFwd γ e α' t)
   in V.Record (α ∧ α') xvs
evalFwd γ (Constr α _ es) α' (T.Constr _ c ts) =
   V.Constr (α ∧ α') c ((\(e' × t) -> evalFwd γ e' α' t) <$> zip es ts)
evalFwd γ (Matrix α e1 _ e2) α' (T.Matrix tss (x × y) (i' × j') t2) =
   case evalFwd γ e2 α' t2 of
      V.Constr _ _ (v1 : v2 : Nil) ->
         let (i'' × β) × (j'' × β') = P.match_fwd v1 × P.match_fwd v2
             vss = assert (i'' == i' && j'' == j') $ A.fromFoldable $ do
                i <- range 1 i'
                singleton $ A.fromFoldable $ do
                   j <- range 1 j'
                   let γ' = M.singleton x (V.Int β i) `disjUnion` (M.singleton y (V.Int β' j))
                   singleton (evalFwd (γ <+> γ') e1 α' (tss!(i - 1)!(j - 1)))
         in V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
      _ -> error absurd
evalFwd γ (Lambda σ) α (T.Lambda _) = V.Closure α (γ `restrict` fv σ) empty σ
evalFwd γ (Project e' _) α (T.Project t xvs' x) =
   case evalFwd γ e' α t of
      V.Record _ xvs -> assert (keys xvs == keys (xvs' # fromFoldable)) $ mustLookup x xvs
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.App (t1 × _ × _) t2 w t3) =
   case evalFwd γ e1 α t1 of
      V.Closure β γ1 δ σ' ->
         let v = evalFwd γ e2 α t2
             γ2 = closeDefsFwd γ1 δ β
             γ3 × e3 × β' = matchFwd v σ' w in
         evalFwd (γ1 <+> γ2 <+> γ3) (asExpr e3) (β ∧ β') t3
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppPrim (t1 × PrimOp φ × _) (t2 × _)) =
   case evalFwd γ e1 α t1 of
      V.Primitive _ vs' ->
         let v2' = evalFwd γ e2 α t2
             vs'' = vs' <> singleton v2' in
         if φ.arity > length vs'' then V.Primitive (PrimOp φ) vs'' else φ.op_fwd vs''
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.AppConstr (t1 × c × _) t2) =
   case evalFwd γ e1 α t1 of
      V.Constr α' _ vs' ->
         let v = evalFwd γ e2 α t2 in
         V.Constr (α ∧ α') c (vs' <> singleton v)
      _ -> error absurd
evalFwd γ (Let (VarDef σ e1) e2) α (T.Let (T.VarDef w t1) t2) =
   let v = evalFwd γ e1 α t1
       γ' × _ × α' = matchFwd v σ w in
   evalFwd (γ <+> γ') e2 α' t2
evalFwd γ (LetRec xσs e') α (T.LetRec _ t) =
   let γ' = closeDefsFwd γ (fromFoldable xσs) α in
   evalFwd (γ <+> γ') e' α t
evalFwd _ _ _ _ = error absurd
