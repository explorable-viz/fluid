module Fwd where

import Prelude hiding (absurd)
import Data.Array (fromFoldable)
import Data.List (List(..), (:), range, singleton)
import Bindings (Bindings(..), (:+:), (↦), find)
import DataType (cPair)
import Eval (closeDefs)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), VarDef(..), body, varAnon)
import Lattice (𝔹, (∧))
import Primitive (apply_fwd, to)
import Util (type (×), (×), absurd, error, mustLookup, successful)
import Val (Env, Val)
import Val (Val(..)) as V

match_fwd :: Val 𝔹 -> Elim 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
match_fwd v (ElimVar x κ)
   | x == varAnon = Empty × κ × true
   | otherwise    = (Empty :+: x ↦ v) × κ × true
match_fwd (V.Constr α c vs) (ElimConstr κs) =
   let κ = mustLookup c κs
       ρ × κ' × α' = matchArgs_fwd vs κ in
   ρ × κ' × (α ∧ α')
match_fwd v _ = error absurd

matchArgs_fwd :: List (Val 𝔹) -> Cont 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
matchArgs_fwd Nil κ              = Empty × κ × true
matchArgs_fwd (v : vs) (Arg σ)   =
   let ρ  × κ'  × α = match_fwd v σ
       ρ' × κ'' × α' = matchArgs_fwd vs κ' in
   (ρ <> ρ') × κ'' × (α ∧ α')
matchArgs_fwd _ _                = error absurd

eval_fwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Val 𝔹
eval_fwd _ Hole _ = V.Hole
eval_fwd ρ (Expr _ (Var x)) _ =
   successful $ find x ρ
eval_fwd ρ (Expr _ (Op op)) _ =
   successful $ find op ρ
eval_fwd ρ (Expr α (Int n)) α' =
   V.Int (α ∧ α') n
eval_fwd ρ (Expr α (Float n)) α' =
   V.Float (α ∧ α') n
eval_fwd ρ (Expr α (Str str)) α' =
   V.Str (α ∧ α') str
eval_fwd ρ (Expr α (Constr c es)) α' =
   V.Constr (α ∧ α') c $ map (\e -> eval_fwd ρ e α') es
eval_fwd ρ (Expr α (Matrix e (x × y) e')) α' =
   case eval_fwd ρ e' α of
      V.Hole -> V.Hole
      (V.Constr _ c (v1 : v2 : Nil)) | c == cPair ->
         let i' × j' = to v1 × to v2
             vs = fromFoldable $ do
                  i <- range 1 i'
                  singleton $ fromFoldable $ do
                     j <- range 1 j'
                     singleton $ eval_fwd ((ρ :+: x ↦ V.Int α i) :+: y ↦ V.Int α j) e α'
         in V.Matrix (α ∧ α') vs (i' × j')
      _ -> error absurd
eval_fwd ρ (Expr _ (LetRec δ e)) α =
   let ρ' = closeDefs ρ δ δ in
   eval_fwd (ρ <> ρ') e α
eval_fwd ρ (Expr _ (Lambda σ)) α = V.Closure ρ Empty σ
eval_fwd ρ (Expr _ (App e e')) α =
   case eval_fwd ρ e α × eval_fwd ρ e' α of
      V.Hole × _           -> V.Hole
      V.Closure ρ1 δ σ × v ->
         let ρ2 = closeDefs ρ1 δ δ
             ρ3 × e'' × β = match_fwd v σ in
         eval_fwd (ρ1 <> ρ2 <> ρ3) (body e'') β
      V.Primitive α' φ × v    -> apply_fwd φ α' v
      V.Constr α' c vs × v -> V.Constr (α ∧ α') c $ vs <> singleton v
      _ × _                -> error absurd
eval_fwd ρ (Expr _ (BinaryApp e1 op e2)) α =
   case successful $ find op ρ of
      V.Hole         -> V.Hole
      V.Primitive α' φ  ->
         case apply_fwd φ α' (eval_fwd ρ e1 α) of
            V.Hole               -> V.Hole
            V.Primitive α'' φ_v  -> apply_fwd φ_v α'' $ eval_fwd ρ e2 α
            _                    -> error absurd
      _                          -> error absurd
eval_fwd ρ (Expr _ (Let (VarDef σ e) e')) α =
   let ρ' × _ × α' = match_fwd (eval_fwd ρ e α) σ in
   eval_fwd (ρ <> ρ') e' α'
