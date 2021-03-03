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
import Lattice (𝔹, (∧))
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
eval_fwd ρ (Var x) _ _                    = successful (find x ρ)
eval_fwd ρ (Op op) _ _                    = successful (find op ρ)
eval_fwd ρ (Int α n) α' _                 = V.Int (α ∧ α') n
eval_fwd ρ (Float α n) α' _               = V.Float (α ∧ α') n
eval_fwd ρ (Str α str) α' _               = V.Str (α ∧ α') str
eval_fwd ρ (Constr α c es) α' (T.Constr _ _ ts) =
   V.Constr (α ∧ α') c ((\(e × t) -> eval_fwd ρ e α' t) <$> zip es ts)
eval_fwd ρ (Matrix α e (x × y) e') α' (T.Matrix tss _ _ t') =
   case eval_fwd ρ e' α t' of
      V.Hole -> V.Hole
      V.Constr _ c (v1 : v2 : Nil) | c == cPair ->
         let i' × j' = to v1 × to v2
             vs = fromFoldable $ do
                  i <- range 1 i'
                  singleton $ fromFoldable $ do
                     j <- range 1 j'
                     singleton (eval_fwd ((ρ :+: x ↦ V.Int α i) :+: y ↦ V.Int α j) e α' (tss!(i - 1)!(j - 1)))
         in V.Matrix (α ∧ α') vs (i' × j')
      _ -> error absurd
eval_fwd ρ (LetRec δ e) α (T.LetRec _ t)  =
   let ρ' = closeDefs ρ δ δ in
   eval_fwd (ρ <> ρ') e α t
eval_fwd ρ (Lambda σ) _ _                 = V.Closure ρ Empty σ
eval_fwd ρ (App e e') α (T.App (t × _) t' _ t'') =
   case eval_fwd ρ e α t × eval_fwd ρ e' α t' of
      V.Hole × _ -> V.Hole
      V.Closure ρ1 δ σ × v ->
         let ρ2 = closeDefs ρ1 δ δ
             ρ3 × e'' × β = match_fwd v σ in
         eval_fwd (ρ1 <> ρ2 <> ρ3) (asExpr e'') β t''
      V.Primitive α' φ × v -> apply_fwd φ α' v
      V.Constr α' c vs × v -> V.Constr (α ∧ α') c (vs <> singleton v)
      _ × _ -> error absurd
eval_fwd ρ (BinaryApp e1 op e2) α (T.BinaryApp (t1 × _) _ (t2 × _)) =
   case successful (find op ρ) of
      V.Hole -> V.Hole
      V.Primitive α' φ ->
         case apply_fwd φ α' (eval_fwd ρ e1 α t1) of
            V.Hole -> V.Hole
            V.Primitive α'' φ_v -> apply_fwd φ_v α'' (eval_fwd ρ e2 α t2)
            _ -> error absurd
      _ -> error absurd
eval_fwd ρ (Let (VarDef σ e) e') α (T.Let (T.VarDef _ t) t') =
   let ρ' × _ × α' = match_fwd (eval_fwd ρ e α t) σ in
   eval_fwd (ρ <> ρ') e' α' t'
eval_fwd _ _ _ _                          = error "todo"
