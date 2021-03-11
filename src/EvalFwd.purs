module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable) as A
import Data.List (List(..), (:), range, singleton, zip)
import Data.Map (fromFoldable)
import Bindings (Bindings(..), (:+:), (↦), find, varAnon)
import DataType (cPair)
import Eval (closeDefs)
import Expl (Expl, Match)
import Expl (Expl(..), Match(..), VarDef(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), asExpr)
import Lattice (𝔹, (∧), botOf, expand)
import Primitive (apply_fwd, from)
import Util (type (×), (×), (!), absurd, error, mustLookup, successful)
import Val (Env, Val)
import Val (Val(..)) as V

match_fwd :: Val 𝔹 -> Elim 𝔹 -> Match 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
match_fwd v σ (T.MatchVar x) =
   case expand σ (ElimVar x ContHole) of
      ElimVar _ κ -> (Empty :+: x ↦ v) × κ × true
      _ -> error absurd
match_fwd _ σ (T.MatchVarAnon _) =
   case expand σ (ElimVar varAnon ContHole) of
      ElimVar _ κ -> Empty × κ × true
      _ -> error absurd
match_fwd v σ (T.MatchConstr c ws cs) =
   case expand v (V.Constr false c (const V.Hole <$> ws)) ×
        expand σ (ElimConstr (fromFoldable ((_ × ContHole) <$> c : cs))) of
      V.Constr α _ vs × ElimConstr m ->
         ρ × κ × (α ∧ α')
         where ρ × κ × α' = matchArgs_fwd vs (mustLookup c m) ws
      _ -> error absurd

matchArgs_fwd :: List (Val 𝔹) -> Cont 𝔹 -> List (Match 𝔹) -> Env 𝔹 × Cont 𝔹 × 𝔹
matchArgs_fwd Nil κ Nil = Empty × κ × true
matchArgs_fwd (v : vs) κ (w : ws) =
   case expand κ (ContElim ElimHole) of
      ContElim σ ->
         (ρ <> ρ') × κ' × (α ∧ α')
         where ρ  × κ  × α    = match_fwd v σ w
               ρ' × κ' × α'   = matchArgs_fwd vs κ ws
      _ -> error absurd
matchArgs_fwd _ _ _ = error absurd

eval_fwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Expl 𝔹 -> Val 𝔹
eval_fwd ρ e _ (T.Var _ x) =
   case expand e (Var x) of
      Var _ -> successful (find x ρ)
      _ -> error absurd
eval_fwd ρ e _ (T.Op _ op) =
   case expand e (Op op) of
      Op _ -> successful (find op ρ)
      _ -> error absurd
eval_fwd ρ e α' (T.Int _ n) =
   case expand e (Int false n) of
      Int α _ -> V.Int (α ∧ α') n
      _ -> error absurd
eval_fwd ρ e α' (T.Float _ n) =
   case expand e (Float false n) of
      Float α _ -> V.Float (α ∧ α') n
      _ -> error absurd
eval_fwd ρ e α' (T.Str _ str) =
   case expand e (Str false str) of
      Str α _ -> V.Str (α ∧ α') str
      _ -> error absurd
eval_fwd ρ e α' (T.Constr _ c ts) =
   case expand e (Constr false c (const Hole <$> ts)) of
      Constr α _ es ->
         V.Constr (α ∧ α') c ((\(e' × t) -> eval_fwd ρ e' α' t) <$> zip es ts)
      _ -> error absurd
eval_fwd ρ e α' (T.Matrix tss (x × y) _ t2) =
   case expand e (Matrix false Hole (x × y) Hole) of
      Matrix α e1 _ e2 ->
         case expand (eval_fwd ρ e2 α t2) (V.Constr false cPair (V.Hole : V.Hole : Nil)) of
            V.Constr _ c (v1 : v2 : Nil) ->
               let (i' × β) × (j' × β') = from v1 × from v2
                   vss = A.fromFoldable $ do
                        i <- range 1 i'
                        singleton $ A.fromFoldable $ do
                           j <- range 1 j'
                           singleton (eval_fwd ((ρ :+: x ↦ V.Int α i) :+: y ↦ V.Int α j) e1 α' (tss!(i - 1)!(j - 1)))
               in V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
            _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.LetRec δ t) =
   case expand e (LetRec (botOf δ) Hole) of
      LetRec δ' e' ->
         let ρ' = closeDefs ρ δ' δ' in
         eval_fwd (ρ <> ρ') e' α t
      _ -> error absurd
eval_fwd ρ e _ (T.Lambda _ _) =
   case expand e (Lambda ElimHole) of
      Lambda σ -> V.Closure ρ Empty σ
      _ -> error absurd
eval_fwd ρ e α (T.App (t1 × ρ1 × δ × σ) t2 w t3) =
   case expand e (App Hole Hole) of
      App e1 e2 ->
         case expand (eval_fwd ρ e1 α t1) (V.Closure (botOf ρ1) (botOf δ) ElimHole) of
            V.Closure ρ1' δ' σ' ->
               let v = eval_fwd ρ e2 α t2
                   ρ2 = closeDefs ρ1' δ' δ'
                   ρ3 × e3 × β = match_fwd v σ' w in
               eval_fwd (ρ1' <> ρ2 <> ρ3) (asExpr e3) β t3
            _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.AppPrim (t1 × φ) (t2 × v2)) =
   case expand e (App Hole Hole) of
      App e1 e2 ->
         apply_fwd (eval_fwd ρ e1 α t1 × φ) (eval_fwd ρ e2 α t2 × v2)
      _ -> error absurd
eval_fwd ρ e α (T.AppConstr (t1 × c × vs) (t2 × _)) =
   case expand e (App Hole Hole) of
      App e1 e2 ->
         case expand (eval_fwd ρ e1 α t1) (V.Constr false c (const V.Hole <$> vs)) of
            V.Constr α' _ vs' ->
               let v = eval_fwd ρ e2 α t2 in
               V.Constr (α ∧ α') c (vs' <> singleton v)
            _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.BinaryApp (t1 × v1) (op × φ) φ_v (t2 × v2)) =
   case expand e (BinaryApp Hole op Hole) of
      BinaryApp e1 _ e2 ->
         apply_fwd (apply_fwd (successful (find op ρ) × φ) (eval_fwd ρ e1 α t1 × v1) × φ_v)
                   (eval_fwd ρ e2 α t2 × v2)
      _ -> error absurd
eval_fwd ρ e α (T.Let (T.VarDef w t1) t2) =
   case expand e (Let (VarDef ElimHole Hole) Hole) of
      Let (VarDef σ e1) e2 ->
         let v = eval_fwd ρ e1 α t1
             ρ' × _ × α' = match_fwd v σ w in
         eval_fwd (ρ <> ρ') e2 α' t2
      _ -> error absurd
