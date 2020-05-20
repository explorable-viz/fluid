module Fwd where

import Prelude (($), (<>))
import Data.Maybe (Maybe(..))
import Bindings ((:+:), (↦), ε, find)
import Expr (Elim(..), Expr, RawExpr(..), T3(..))
import Primitive (opFun)
import Selected (Selected(..), (∧))
import Util (absurd, error)
import Val (Env, Val, toValues_fwd)
import Val (RawVal(..)) as V


match_fwd :: Val -> Elim -> Maybe (T3 Env Expr Selected)
match_fwd v (ElimVar { x, e }) = Just $ T3 (ε :+: x ↦ v) e Top
match_fwd { α, u: V.True } (ElimBool { true: e, false: _ }) = Just $ T3 ε e α
match_fwd { α, u: V.False } (ElimBool { true: _, false: e }) = Just $ T3 ε e α
match_fwd { α, u: V.Pair u v } (ElimPair { x, y, e }) = Just $ T3 (ε :+: x ↦ u :+: y ↦ v) e α
match_fwd { α, u: V.Nil } (ElimList { nil: e, cons: _ }) = Just $ T3 ε e α
match_fwd { α, u: V.Cons u v } (ElimList { nil: _, cons: { x, y, e } }) = Just $ T3 (ε :+: x ↦ u :+: y ↦ v) e Top
match_fwd _ _ = Nothing

eval_fwd :: Env -> Expr -> Selected -> Val
eval_fwd ρ { r: Var x } _ =
   case find x ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ { r: Op op } _ =
   case find op ρ of
      Just v -> v
      _ -> error absurd
eval_fwd ρ { α, r: True } α' = { α: α ∧ α', u: V.True }
eval_fwd ρ { α, r: False } α' = { α: α ∧ α', u: V.False }
eval_fwd ρ { α, r: Int n } α' = { α: α ∧ α', u: V.Int n }
eval_fwd ρ { α, r: Pair e1 e2 } α' = { α: α ∧ α', u: V.Pair (eval_fwd ρ e1 α') (eval_fwd ρ e2 α') }
eval_fwd ρ { α, r: Nil} α' = { α: α ∧ α', u: V.Nil }
eval_fwd ρ { α, r: Cons e e' } α' = { α: α ∧ α', u: V.Cons (eval_fwd ρ e α') (eval_fwd ρ e' α') }
eval_fwd ρ { r: Letrec f σ e } α = eval_fwd (ρ :+: f ↦ { α, u: V.Closure ρ f σ }) e α
eval_fwd ρ { r: App e e' } α =
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
eval_fwd ρ { r: BinaryApp e1 op e2 } α =
   case find op ρ of
      Just { α: α', u: V.Op φ } -> toValues_fwd (opFun φ) α' (eval_fwd ρ e1 α) (eval_fwd ρ e2 α)
      _ -> error absurd
eval_fwd ρ { r: Let x e1 e2 } α = eval_fwd (ρ :+: x ↦ eval_fwd ρ e1 α) e2 α
eval_fwd ρ { r: Match e σ } α =
   case match_fwd (eval_fwd ρ e α) σ of
      Just (T3 ρ' e' α') -> eval_fwd (ρ <> ρ') e' α'
      Nothing -> error absurd
