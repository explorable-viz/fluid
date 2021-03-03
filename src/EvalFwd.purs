module EvalFwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable)
import Data.List (List(..), (:), range, singleton, zip)
import Bindings (Bindings(..), (:+:), (↦), find, varAnon)
import DataType (cPair)
import Eval (closeDefs)
import Expl (Expl)
import Expl (Expl(..), VarDef(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), asExpr)
import Lattice (𝔹, (∧), botOf, expand)
import Primitive (apply_fwd, to)
import Util (type (×), (×), (!), absurd, error, mustLookup, successful)
import Val (Env, Val)
import Val (Val(..)) as V

match_fwd :: Val 𝔹 -> Elim 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
match_fwd _ ElimHole                         = error "todo"
match_fwd v (ElimVar x κ)
   | x == varAnon                            = Empty × κ × true
   | otherwise                               = (Empty :+: x ↦ v) × κ × true
match_fwd (V.Constr α c vs) (ElimConstr κs)  = ρ × κ × (α ∧ α')
   where ρ × κ × α' = matchArgs_fwd vs (mustLookup c κs)
match_fwd V.Hole (ElimConstr _)              = error "todo"
match_fwd _ (ElimConstr _)                   = error absurd

matchArgs_fwd :: List (Val 𝔹) -> Cont 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchArgs_fwd Nil κ                 = Empty × κ × true
matchArgs_fwd (v : vs) (ContElim σ) = (ρ <> ρ') × κ' × (α ∧ α')
   where ρ  × κ  × α = match_fwd v σ
         ρ' × κ' × α' = matchArgs_fwd vs κ
matchArgs_fwd _ _ = error absurd

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
         case eval_fwd ρ e2 α t2 of
            V.Hole -> V.Hole
            V.Constr _ c (v1 : v2 : Nil) | c == cPair ->
               let i' × j' = to v1 × to v2
                   vs = fromFoldable $ do
                        i <- range 1 i'
                        singleton $ fromFoldable $ do
                           j <- range 1 j'
                           singleton (eval_fwd ((ρ :+: x ↦ V.Int α i) :+: y ↦ V.Int α j) e1 α' (tss!(i - 1)!(j - 1)))
               in V.Matrix (α ∧ α') vs (i' × j')
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
eval_fwd ρ e α (T.App (t1 × _) t2 _ t3) =
   case expand e (App Hole Hole) of
      App e1 e2 ->
         case eval_fwd ρ e1 α t1 × eval_fwd ρ e2 α t2 of
            V.Hole × _ -> V.Hole
            V.Closure ρ1 δ σ × v ->
               let ρ2 = closeDefs ρ1 δ δ
                   ρ3 × e3 × β = match_fwd v σ in
               eval_fwd (ρ1 <> ρ2 <> ρ3) (asExpr e3) β t3
            _ × _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.AppOp (t1 × _) (t2 × _)) =
   case expand e (App Hole Hole) of
      App e1 e2 ->
         case eval_fwd ρ e1 α t1 × eval_fwd ρ e2 α t2 of
            V.Hole × _ -> V.Hole
            V.Primitive α' φ × v -> apply_fwd φ α' v
            V.Constr α' c vs × v -> V.Constr (α ∧ α') c (vs <> singleton v)
            _ × _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.BinaryApp (t1 × _) (op × _) (t2 × _)) =
   case expand e (BinaryApp Hole op Hole) of
      BinaryApp e1 _ e2 ->
         case successful (find op ρ) of
            V.Hole -> V.Hole
            V.Primitive α' φ ->
               case apply_fwd φ α' (eval_fwd ρ e1 α t1) of
                  V.Hole -> V.Hole
                  V.Primitive α'' φ_v -> apply_fwd φ_v α'' (eval_fwd ρ e2 α t2)
                  _ -> error absurd
            _ -> error absurd
      _ -> error absurd
eval_fwd ρ e α (T.Let (T.VarDef _ t1) t2) =
   case expand e (Let (VarDef ElimHole Hole) Hole) of
      Let (VarDef σ e1) e2 ->
         let ρ' × _ × α' = match_fwd (eval_fwd ρ e1 α t1) σ in
         eval_fwd (ρ <> ρ') e2 α' t2
      _ -> error absurd
