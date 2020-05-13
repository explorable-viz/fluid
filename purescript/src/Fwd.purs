module Fwd where

import Prelude (($), (<>))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bindings(..), (:+:), (↦), find)
import Expr (Elim(..), Expr, RawExpr(..), T3(..))
import Selected (Selected(..), (∧))
import Util (absurd)
import Val (Env, Val)
import Val (RawVal(..)) as V


fwd_match :: Val -> Elim -> Maybe (T3 Env Expr Selected)
-- var
fwd_match v (ElimVar { x, e }) = Just $ T3 (Empty :+: x ↦ v) e Top
-- true
fwd_match { α, u: V.True } (ElimBool { true: e, false: _ }) = Just $ T3 Empty e α
-- false
fwd_match { α, u: V.False } (ElimBool { true: _, false: e }) = Just $ T3 Empty e α
-- pair
fwd_match { α, u: V.Pair u v } (ElimPair { x, y, e }) =
   Just $ T3 (Empty :+: x ↦ u :+: y ↦ v) e α
-- nil
fwd_match { α, u: V.Nil } (ElimList { nil: e, cons: _ }) = Just $ T3 Empty e α
-- cons
fwd_match { α, u: V.Cons u v } (ElimList { nil: _, cons: { x, y, e } }) =
   Just $ T3 (Empty :+: x ↦ u :+: y ↦ v) e Top
-- failure
fwd_match _ _ =  Nothing

fwd :: Env -> Expr -> Selected -> Val
-- var
fwd ρ { r: Var x } _ =
   case find x ρ of
      Just val -> val
      _ -> absurd
-- true
fwd ρ { α, r: True } α' = { α: α ∧ α', u: V.True }
-- false
fwd ρ { α, r: False } α' = { α: α ∧ α', u: V.False }
-- int
fwd ρ { α, r: Int n } α' = { α: α ∧ α', u: V.Int n }
-- pair
fwd ρ { α, r: Pair e1 e2 } α' = { α: α ∧ α', u: V.Pair (fwd ρ e1 α') (fwd ρ e2 α') }
-- nil
fwd ρ { α, r: Nil} α' = { α: α ∧ α', u: V.Nil }
-- cons
fwd ρ { α, r: Cons e e' } α' = { α: α ∧ α', u: V.Cons (fwd ρ e α') (fwd ρ e' α') }
-- op
fwd ρ { α, r: Op op } α' = { α: α ∧ α', u: V.Op op }
-- letrec
fwd ρ { r: Letrec f σ e } α = fwd (ρ :+: f ↦ { α, u: V.Closure ρ f σ }) e α
-- app
fwd ρ { r: App e e' } α =
   case fwd ρ e α  of
      { α: α', u: V.Closure ρ' f σ } ->
         case fwd_match (fwd ρ e' α) σ of
            Just (T3 ρ'' e'' α'') ->
               let ρ_f = (ρ' <> ρ'') :+: f ↦ { α: α', u: (V.Closure ρ' f σ) } in fwd ρ_f e'' (α' ∧ α'')
            Nothing -> absurd
      _  -> absurd
-- binary app
fwd ρ { r: BinaryApp op e1 e2 } α =
   case fwd ρ e1 α, fwd ρ e2 α of
   { α: α', u: V.Int n1 }, { α: α'', u: V.Int n2 } -> { α: α ∧ α' ∧ α'', u: V.Int (n1 + n2) }
   _, _ -> absurd
-- let
fwd ρ { r: Let x e1 e2 } α =
   fwd (ρ :+: x ↦ fwd ρ e1 α) e2 α
-- match
fwd ρ { r: Match e σ } α =
   case fwd_match (fwd ρ e α) σ of
      Just (T3 ρ' e' α') -> fwd (ρ <> ρ') e' α'
      Nothing -> absurd
-- otherwise
fwd _ _ _ = absurd
