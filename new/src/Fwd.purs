module Fwd where

import Prelude hiding (absurd)
import Data.Maybe (Maybe(..))
import Bindings ((:+:), (↦), ε, find)
import Expr (Elim(..), Expr(..), RawExpr(..), T3(..))
import Primitive (opFun)
import Selected (Selected(..), (∧))
import Util (absurd, error)
import Val (Env, Val, toValues_fwd)
import Val (RawVal(..)) as V


match_fwd :: forall k . Val -> Elim k -> Maybe (T3 Env k Selected)
match_fwd v (ElimVar x κ) = Just $ T3 (ε :+: x ↦ v) κ Top
match_fwd { α, u: V.True } (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ α
match_fwd { α, u: V.False } (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ' α
match_fwd { α, u: V.Pair v v' } (ElimPair σ) = do
   T3 ρ1 τ α' <- match_fwd v σ
   T3 ρ κ α'' <- match_fwd v' τ
   pure $ T3 (ρ1 <> ρ) κ (α' ∧ α'')
match_fwd { α, u: V.Nil } (ElimList { nil: κ, cons: σ }) = Just $ T3 ε κ α
match_fwd { α, u : V.Cons v v' } (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ α' <- match_fwd v σ
   T3 ρ κ' α'' <- match_fwd v' τ
   pure $ T3 (ρ1 <> ρ) κ' (α' ∧ α'')
match_fwd _ _ = Nothing

eval_fwd :: Env -> Expr -> Selected -> Val
eval_fwd ρ (Expr _ (Var x)) _ =
   case find x ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ (Expr _ (Op op)) _ =
   case find op ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ (Expr α True) α' = { α: α ∧ α', u: V.True }
eval_fwd ρ (Expr α False) α' = { α: α ∧ α', u: V.False }
eval_fwd ρ (Expr α (Int n)) α' = { α: α ∧ α', u: V.Int n }
eval_fwd ρ (Expr α (Pair e1 e2)) α' = { α: α ∧ α', u: V.Pair (eval_fwd ρ e1 α') (eval_fwd ρ e2 α') }
eval_fwd ρ (Expr α Nil) α' = { α: α ∧ α', u: V.Nil }
eval_fwd ρ (Expr α (Cons e e')) α' = { α: α ∧ α', u: V.Cons (eval_fwd ρ e α') (eval_fwd ρ e' α') }
eval_fwd ρ (Expr _ (Letrec f σ e)) α = eval_fwd (ρ :+: f ↦ { α, u: V.Closure ρ f σ }) e α
eval_fwd ρ (Expr _ (App e e')) α =
   case eval_fwd ρ e α, eval_fwd ρ e' α of
      { α: α', u: V.Closure ρ' f σ }, v ->
         case match_fwd v σ of
            Just (T3 ρ'' e'' α'') ->
               let ρ_f = (ρ' <> ρ'') :+: f ↦ { α: α', u: (V.Closure ρ' f σ) } in
               eval_fwd ρ_f e'' (α' ∧ α'')
            Nothing -> error absurd
      { α: α', u: V.Op op }, v -> { α: α', u: V.PartialApp op v }
      { α: α', u: V.PartialApp op v }, v' -> toValues_fwd (opFun op) α' v v'
      _, _ -> error absurd
eval_fwd ρ (Expr _ (BinaryApp e1 op e2)) α =
   case find op ρ of
      Just { α: α', u: V.Op φ } -> toValues_fwd (opFun φ) α' (eval_fwd ρ e1 α) (eval_fwd ρ e2 α)
      _ -> error absurd
eval_fwd ρ (Expr _ (Let x e1 e2)) α = eval_fwd (ρ :+: x ↦ eval_fwd ρ e1 α) e2 α
eval_fwd ρ (Expr _ (Match e σ)) α =
   case match_fwd (eval_fwd ρ e α) σ of
      Just (T3 ρ' e' α') -> eval_fwd (ρ <> ρ') e' α'
      Nothing -> error absurd
