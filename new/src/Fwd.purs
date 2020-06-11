module Fwd where

import Prelude hiding (absurd)
import Data.List (List(..), (:))
import Bindings ((:+:), (↦), ε, find)
import Elim (Elim(..))
import Expr (Def(..), Expr(..), RecDef(..), RecDefs)
import Expr (RawExpr(..)) as E
import Lattice (Selected, (∧))
import Primitive (applyBinary_fwd, applyUnary_fwd)
import Util (T3(..), absurd, error, successful)
import Val (Env, UnaryOp(..), Val(..))
import Val (RawVal(..)) as V

match_fwd :: forall k . Val -> Elim k -> T3 Env k Selected
match_fwd v (ElimVar x κ) =
   T3 (ε :+: x ↦ v) κ true
match_fwd (Val α V.True) (ElimBool { true: κ, false: κ' }) =
   T3 ε κ α
match_fwd (Val α V.False) (ElimBool { true: κ, false: κ' }) =
   T3 ε κ' α
match_fwd (Val α (V.Pair v v')) (ElimPair σ) =
   let T3 ρ1 τ α' = match_fwd v σ
       T3 ρ2 κ α'' = match_fwd v' τ in
   T3 (ρ1 <> ρ2) κ (α' ∧ α'')
match_fwd (Val α V.Nil) (ElimList { nil: κ, cons: σ }) =
   T3 ε κ α
match_fwd (Val α (V.Cons v v')) (ElimList { nil: κ, cons: σ }) =
   let T3 ρ1 τ α' = match_fwd v σ
       T3 ρ κ' α'' = match_fwd v' τ in
   T3 (ρ1 <> ρ) κ' (α' ∧ α'')
match_fwd _ _ =
   error absurd

closeDefs_fwd :: Env -> RecDefs -> RecDefs -> Selected -> Env
closeDefs_fwd _ _ Nil _ = ε
closeDefs_fwd ρ δ0 (RecDef f σ : δ) α =
   closeDefs_fwd ρ δ0 δ α :+: f ↦ Val α (V.Closure ρ δ0 σ)

eval_fwd :: Env -> Expr -> Selected -> Val
eval_fwd ρ (Expr _ (E.Var x)) _ =
   successful $ find x ρ
eval_fwd ρ (Expr _ (E.Op op)) _ =
   successful $ find op ρ
eval_fwd ρ (Expr α E.True) α' =
   Val (α ∧ α') V.True
eval_fwd ρ (Expr α E.False) α' =
   Val (α ∧ α') V.False
eval_fwd ρ (Expr α (E.Int n)) α' =
   Val (α ∧ α') $ V.Int n
eval_fwd ρ (Expr α (E.Str str)) α' =
   Val (α ∧ α') $ V.Str str
eval_fwd ρ (Expr α (E.Constr c es)) α' =
   Val (α ∧ α') $ V.Constr c $ map (\e -> eval_fwd ρ e α') es
eval_fwd ρ (Expr α (E.Pair e1 e2)) α' =
   Val (α ∧ α') $ V.Pair (eval_fwd ρ e1 α') (eval_fwd ρ e2 α')
eval_fwd ρ (Expr α E.Nil) α' =
   Val (α ∧ α') V.Nil
eval_fwd ρ (Expr α (E.Cons e e')) α' =
   Val (α ∧ α') $ V.Cons (eval_fwd ρ e α') (eval_fwd ρ e' α')
eval_fwd ρ (Expr _ (E.LetRec δ e)) α =
   let ρ' = closeDefs_fwd ρ δ δ α in
   eval_fwd (ρ <> ρ') e α
eval_fwd ρ (Expr _ (E.Lambda σ)) α = Val α $ V.Closure ρ Nil σ
eval_fwd ρ (Expr _ (E.App e e')) α =
   let Val α' u = eval_fwd ρ e α
       v = eval_fwd ρ e' α in
   case u of
      V.Closure ρ1 δ σ ->
         let ρ2 = closeDefs_fwd ρ1 δ δ α'
             T3 ρ3 e'' α'' = match_fwd v σ in
         eval_fwd (ρ1 <> ρ2 <> ρ3) e'' (α' ∧ α'')
      V.Unary φ -> applyUnary_fwd φ α' v
      V.Binary φ -> Val α' $ V.Unary $ PartialApp φ v
      _ -> error absurd
eval_fwd ρ (Expr _ (E.BinaryApp e1 op e2)) α =
   case successful (find op ρ) of
      Val α' (V.Binary φ) -> eval_fwd ρ e1 α `applyBinary_fwd φ α'` eval_fwd ρ e2 α
      _ -> error absurd
eval_fwd ρ (Expr _ (E.Let (Def σ e) e')) α =
   let T3 ρ' _ α' = match_fwd (eval_fwd ρ e α) σ in
   eval_fwd (ρ <> ρ') e' α'
eval_fwd ρ (Expr _ (E.MatchAs e σ)) α =
   let T3 ρ' e' α' = match_fwd (eval_fwd ρ e α) σ in
   eval_fwd (ρ <> ρ') e' α'
