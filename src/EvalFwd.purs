module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), length, range, singleton, zip)
import Data.List.NonEmpty (singleton) as NE
import Data.Map (empty, insert)
import Data.Map (singleton) as M
import Data.Profunctor.Strong ((***), (&&&), first, second)
import Data.Set (union)
import Bindings (Bindings, (↦), find, key, val)
import Expl (Expl(..), Match(..), VarDef(..)) as T
import Expl (Expl, Match)
import Expr (Cont, Elim(..), Expr(..), RecDefs, VarDef(..), asElim, asExpr, for, fv)
import Lattice (𝔹, (∧))
import Primitive (match_fwd) as P
import Util (type (×), (×), (!), absurd, assert, error, mustLookup, successful)
import Util.SnocList (SnocList(..), (:-))
import Util.SnocList (unzip, zip, zipWith) as S
import Val (Env2, PrimOp(..), SingletonEnv, Val, concat, disjUnion, lookup', restrict)
import Val (Val(..)) as V

matchFwd :: Val 𝔹 -> Elim 𝔹 -> Match 𝔹 -> SingletonEnv 𝔹 × Cont 𝔹 × 𝔹
matchFwd _ (ElimVar _ κ) (T.MatchVarAnon _) = empty × κ × true
matchFwd v (ElimVar _ κ) (T.MatchVar x) = M.singleton x v × κ × true
matchFwd (V.Constr α _ vs) (ElimConstr m) (T.MatchConstr c ws _) =
   second (_ ∧ α) (matchArgsFwd vs (mustLookup c m) ws)
matchFwd (V.Record α xvs) (ElimRecord _ κ) (T.MatchRecord xws) =
   second (_ ∧ α) (matchRecordFwd xvs κ xws)
matchFwd _ _ _ = error absurd

matchArgsFwd :: List (Val 𝔹) -> Cont 𝔹 -> List (Match 𝔹) -> SingletonEnv 𝔹 × Cont 𝔹 × 𝔹
matchArgsFwd Nil κ Nil = empty × κ × true
matchArgsFwd (v : vs) σ (w : ws) =
   let ρ × κ × α = matchFwd v (asElim σ) w in
   (first (ρ `disjUnion` _) *** (_ ∧ α)) (matchArgsFwd vs κ ws)
matchArgsFwd _ _ _ = error absurd

matchRecordFwd :: Bindings (Val 𝔹) -> Cont 𝔹 -> Bindings (Match 𝔹) -> SingletonEnv 𝔹 × Cont 𝔹 × 𝔹
matchRecordFwd Lin κ Lin = empty × κ × true
matchRecordFwd (xvs :- x ↦ v) σ (xws :- x' ↦ w) | x == x' =
   let ρ × σ' × α = matchRecordFwd xvs σ xws in
   (first (ρ `disjUnion` _) *** (_ ∧ α)) (matchFwd v (asElim σ') w)
matchRecordFwd _ _ _ = error absurd

closeDefsFwd :: Env2 𝔹 -> RecDefs 𝔹 -> 𝔹 -> RecDefs 𝔹 -> SingletonEnv 𝔹
closeDefsFwd _ _ _ Lin = empty
closeDefsFwd γ ρ0 α (ρ :- f ↦ σ) =
   let xs = fv (ρ0 `for` σ) `union` fv σ
   in closeDefsFwd γ ρ0 α ρ # insert f (V.Closure α (γ `restrict` xs) ρ0 σ)

evalFwd :: Env2 𝔹 -> Expr 𝔹 -> 𝔹 -> Expl 𝔹 -> Val 𝔹
evalFwd γ (Var _) _ (T.Var _ x) = successful (lookup' x γ)
evalFwd γ (Op _) _ (T.Op _ op) = successful (lookup' op γ)
evalFwd _ (Int α _) α' (T.Int _ n) = V.Int (α ∧ α') n
evalFwd _ (Float α _) α' (T.Float _ n) = V.Float (α ∧ α') n
evalFwd _ (Str α _) α' (T.Str _ str) = V.Str (α ∧ α') str
evalFwd γ (Record α xes) α' (T.Record _ xts) =
   let xs × ts = xts <#> (key &&& val) # S.unzip
       es = xes <#> val
       vs = (\(e' × t) -> evalFwd γ e' α' t) <$> S.zip es ts in
   V.Record (α ∧ α') (S.zipWith (↦) xs vs)
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
                   singleton (evalFwd (γ `concat` γ') e1 α' (tss!(i - 1)!(j - 1)))
         in V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
      _ -> error absurd
evalFwd γ (LetRec ρ e') α (T.LetRec _ t) =
   let γ' = closeDefsFwd γ ρ α ρ in
   evalFwd (γ `concat` γ') e' α t
evalFwd γ (Lambda σ) α (T.Lambda _ _) = V.Closure α (γ `restrict` fv σ) Lin σ
evalFwd γ (RecordLookup e' _) α (T.RecordLookup t xs x) =
   case evalFwd γ e' α t of
      V.Record _ xvs -> assert ((xvs <#> key) == xs) $ successful (find x xvs)
      _ -> error absurd
evalFwd γ (App e1 e2) α (T.App (t1 × _ × _) t2 w t3) =
   case evalFwd γ e1 α t1 of
      V.Closure β γ1 δ σ' ->
         let v = evalFwd γ e2 α t2
             γ1' = γ1 <#> NE.singleton
             γ2 = closeDefsFwd γ1' δ β δ
             γ3 × e3 × β' = matchFwd v σ' w in
         evalFwd ((γ1' `concat` γ2) `concat` γ3) (asExpr e3) (β ∧ β') t3
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
   evalFwd (γ `concat` γ') e2 α' t2
evalFwd _ _ _ _ = error absurd
