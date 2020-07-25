module Fwd where

import Prelude hiding (absurd)
import Data.List (List(..), (:), singleton)
import Data.Map (lookup)
import Bindings (Bindings(..), (:+:), (↦), find)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), RecDef(..), RecDefs, VarDef(..), body, varAnon)
import Lattice (𝔹, (∧))
import Primitive (apply_fwd)
import Util (type (×), (×), absurd, error, fromJust, successful)
import Val (Env, Val(Val))
import Val (RawVal(..), Val(Hole)) as V

match_fwd :: Val 𝔹 -> Elim 𝔹 -> Env 𝔹 × Cont 𝔹 × 𝔹
match_fwd v (ElimVar x κ)
   | x == varAnon = Empty × κ × true
   | otherwise    = (Empty :+: x ↦ v) × κ × true
match_fwd (Val α (V.Constr c vs)) (ElimConstr κs) =
   let κ = fromJust absurd $ lookup c κs
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

closeDefs_fwd :: Env 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹 -> 𝔹 -> Env 𝔹
closeDefs_fwd _ _ Nil _                = Empty
closeDefs_fwd ρ δ0 (RecDef f σ : δ) α  =
   closeDefs_fwd ρ δ0 δ α :+: f ↦ Val α (V.Closure ρ δ0 σ)

eval_fwd :: Env 𝔹 -> Expr 𝔹 -> 𝔹 -> Val 𝔹
eval_fwd _ Hole _ = V.Hole
eval_fwd ρ (Expr _ (Var x)) _ =
   successful $ find x ρ
eval_fwd ρ (Expr _ (Op op)) _ =
   successful $ find op ρ
eval_fwd ρ (Expr α (Int n)) α' =
   Val (α ∧ α') $ V.Int n
eval_fwd ρ (Expr α (Str str)) α' =
   Val (α ∧ α') $ V.Str str
eval_fwd ρ (Expr α (Constr c es)) α' =
   Val (α ∧ α') $ V.Constr c $ map (\e -> eval_fwd ρ e α') es
eval_fwd ρ (Expr _ (LetRec δ e)) α =
   let ρ' = closeDefs_fwd ρ δ δ α in
   eval_fwd (ρ <> ρ') e α
eval_fwd ρ (Expr _ (Lambda σ)) α = Val α $ V.Closure ρ Nil σ
eval_fwd ρ (Expr _ (App e e')) α =
   case eval_fwd ρ e α of
      V.Hole   -> V.Hole
      Val α' u ->
         let v = eval_fwd ρ e' α in
         case u of
            V.Closure ρ1 δ σ  ->
               let ρ2 = closeDefs_fwd ρ1 δ δ α'
                   ρ3 × e'' × α'' = match_fwd v σ in
               eval_fwd (ρ1 <> ρ2 <> ρ3) (body e'') $ α' ∧ α''
            V.Primitive φ     -> apply_fwd φ α' v
            V.Constr c vs     -> Val (α ∧ α') $ V.Constr c $ vs <> singleton v
            _                 -> error absurd
eval_fwd ρ (Expr _ (BinaryApp e1 op e2)) α =
   case successful $ find op ρ of
      V.Hole                  -> V.Hole
      Val α' (V.Primitive φ)  ->
         case apply_fwd φ α' (eval_fwd ρ e1 α) of
            V.Hole                     -> V.Hole
            Val α'' (V.Primitive φ_v)  -> apply_fwd φ_v α'' $ eval_fwd ρ e2 α
            _                          -> error absurd
      _                       -> error absurd
eval_fwd ρ (Expr _ (Let (VarDef σ e) e')) α =
   let ρ' × _ × α' = match_fwd (eval_fwd ρ e α) σ in
   eval_fwd (ρ <> ρ') e' α'
eval_fwd ρ (Expr _ (MatchAs e σ)) α =
   let ρ' × e' × α' = match_fwd (eval_fwd ρ e α) σ in
   eval_fwd (ρ <> ρ') (body e') α'
