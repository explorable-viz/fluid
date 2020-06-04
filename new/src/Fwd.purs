module Fwd where

import Prelude hiding (absurd)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Bindings ((:+:), (↦), ε, find)
import Expr (Def(..), Def2(..), Elim(..), Expr(..), RecDef(..), RecDefs)
import Expr (RawExpr(..)) as E
import Primitive (applyBinary_fwd, applyUnary_fwd)
import Selected (Selected(..), (∧))
import Util (T3(..), absurd, error)
import Val (Env, UnaryOp(..), Val(..))
import Val (RawVal(..)) as V


match_fwd :: forall k . Val -> Elim k -> Maybe (T3 Env k Selected)
match_fwd v (ElimVar x κ) = Just $ T3 (ε :+: x ↦ v) κ Top
match_fwd (Val α V.True) (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ α
match_fwd (Val α V.False) (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ' α
match_fwd (Val α (V.Pair v v')) (ElimPair σ) = do
   T3 ρ1 τ α' <- match_fwd v σ
   T3 ρ2 κ α'' <- match_fwd v' τ
   pure $ T3 (ρ1 <> ρ2) κ (α' ∧ α'')
match_fwd (Val α V.Nil) (ElimList { nil: κ, cons: σ }) = Just $ T3 ε κ α
match_fwd (Val α (V.Cons v v')) (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ α' <- match_fwd v σ
   T3 ρ κ' α'' <- match_fwd v' τ
   pure $ T3 (ρ1 <> ρ) κ' (α' ∧ α'')
match_fwd _ _ = Nothing

closeDefs_fwd :: Env -> RecDefs -> RecDefs -> Selected -> Env
closeDefs_fwd _ _ Nil _ = ε
closeDefs_fwd ρ δ0 (RecDef f σ : δ) α = closeDefs_fwd ρ δ0 δ α :+: f ↦ Val α (V.Closure ρ δ0 σ)

eval_fwd :: Env -> Expr -> Selected -> Val
eval_fwd ρ (Expr _ (E.Var x)) _ =
   case find x ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ (Expr _ (E.Op op)) _ =
   case find op ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ (Expr α E.True) α' = Val (α ∧ α') V.True
eval_fwd ρ (Expr α E.False) α' = Val (α ∧ α') V.False
eval_fwd ρ (Expr α (E.Int n)) α' = Val (α ∧ α') $ V.Int n
eval_fwd ρ (Expr α (E.Str str)) α' = Val (α ∧ α') $ V.Str str
eval_fwd ρ (Expr α (E.Pair e1 e2)) α' = Val (α ∧ α') $ V.Pair (eval_fwd ρ e1 α') (eval_fwd ρ e2 α')
eval_fwd ρ (Expr α E.Nil) α' = Val (α ∧ α') V.Nil
eval_fwd ρ (Expr α (E.Cons e e')) α' = Val (α ∧ α') $ V.Cons (eval_fwd ρ e α') (eval_fwd ρ e' α')
eval_fwd ρ (Expr _ (E.LetRec δ e)) α =
   let ρ' = closeDefs_fwd ρ δ δ α in
   eval_fwd (ρ <> ρ') e α
eval_fwd ρ (Expr _ (E.Lambda σ)) α = Val α $ V.Closure ρ Nil σ
eval_fwd ρ (Expr _ (E.App e e')) α =
   case eval_fwd ρ e α, eval_fwd ρ e' α of
      Val α' (V.Closure ρ1 δ σ), v ->
         let ρ2 = closeDefs_fwd ρ1 δ δ α' in
         case match_fwd v σ of
            Just (T3 ρ3 e'' α'') -> eval_fwd (ρ1 <> ρ2 <> ρ3) e'' (α' ∧ α'')
            Nothing -> error absurd
      Val α' (V.Unary φ), v -> applyUnary_fwd φ α' v
      Val α' (V.Binary φ), v -> Val α' $ V.Unary $ PartialApp φ v
      _, _ -> error absurd
eval_fwd ρ (Expr _ (E.BinaryApp e1 op e2)) α =
   case find op ρ of
      Just (Val α' (V.Binary φ)) -> eval_fwd ρ e1 α `applyBinary_fwd φ α'` eval_fwd ρ e2 α
      _ -> error absurd
eval_fwd ρ (Expr _ (E.Let (Def x e) e')) α =
   eval_fwd (ρ :+: x ↦ eval_fwd ρ e α) e' α
eval_fwd ρ (Expr _ (E.Let2 (Def2 σ e) e')) α =
   case match_fwd (eval_fwd ρ e α) σ of
      Just (T3 ρ' _ α') -> eval_fwd (ρ <> ρ') e' α'
      Nothing -> error absurd
eval_fwd ρ (Expr _ (E.MatchAs e σ)) α =
   case match_fwd (eval_fwd ρ e α) σ of
      Just (T3 ρ' e' α') -> eval_fwd (ρ <> ρ') e' α'
      Nothing -> error absurd
