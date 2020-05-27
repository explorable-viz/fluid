module Eval where

import Prelude hiding (absurd)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Def(..), Defs, Elim(..), Expr(..), T3(..))
import Expr (RawExpr(..)) as E
import Primitive (opFun)
import Util (absurd, error)
import Val (Env, Val, toValues, val)
import Val (RawVal(..)) as V

match :: forall k . Val -> Elim k -> Maybe (T3 Env k (Match k))
match v (ElimVar x κ) = Just $ T3 (ε :+: x ↦ v) κ (MatchVar x)
match { u: V.True } (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ (MatchTrue κ')
match { u: V.False } (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ' (MatchFalse κ)
match { u: V.Pair v v' } (ElimPair σ) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ2 κ ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ2) κ (MatchPair ξ ξ')
match { u: V.Nil } (ElimList { nil: κ, cons: σ }) = Just $ T3 ε κ (MatchNil σ)
match { u : V.Cons v v' } (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ κ' ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ) κ' (MatchCons { nil: κ, cons: Tuple ξ ξ' })
match _ _ = Nothing

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> Defs -> Defs -> Env
closeDefs _ _ Nil = ε
closeDefs ρ δ0 (Cons (Def f σ) δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ σ)

type ExplVal = { t :: Expl, v :: Val }

eval :: Env -> Expr -> ExplVal
eval ρ (Expr _ (E.Var x)) =
   case find x ρ of
      Just v -> { t: T.Var x, v }
      _ -> error $ "variable " <> x <> " not found"
eval ρ (Expr _ (E.Op op)) =
   case find op ρ of
      Just v -> { t: T.Op op, v }
      _ -> error $ "operator " <> op <> " not found"
eval ρ (Expr _ E.True) = { t: T.True, v: val V.True }
eval ρ (Expr _ E.False) = { t: T.False, v: val V.False }
eval ρ (Expr _ (E.Int n)) = { t: T.Int n, v: val $ V.Int n }
eval ρ (Expr _ (E.Pair e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
      { t: T.Pair t t', v: val $ V.Pair v v' }
eval ρ (Expr _ E.Nil) = { t: T.Nil, v: val V.Nil }
eval ρ (Expr _ (E.Cons e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
   { t: T.Cons t t', v: val $ V.Cons v v' }
eval ρ (Expr _ (E.Letrec δ e)) =
   let ρ' = closeDefs ρ δ δ
       { t, v } = eval (ρ <> ρ') e in
   { t: T.Letrec δ t, v }
eval ρ (Expr _ (E.App e e')) =
   case eval ρ e, eval ρ e' of
      { t, v: { u: V.Closure ρ1 δ σ } }, { t: t', v } ->
         let ρ2 = closeDefs ρ1 δ δ in
         case match v σ of
            Just (T3 ρ3 e'' ξ) ->
               let { t: u, v: v' } = eval (ρ1 <> ρ2 <> ρ3) e''
               in { t: T.App t t' ξ u, v: v' }
            Nothing -> error "Pattern mismatch"
      { t, v: { u: V.Op op } }, { t: t', v } ->
         { t: T.AppOp t t', v: val $ V.PartialApp op v }
      { t, v: { u: V.PartialApp op v } }, { t: t', v: v' } ->
         { t: T.AppOp t t', v: toValues (opFun op) v v' }
      _, _ -> error "Expected closure or operator"
eval ρ (Expr _ (E.BinaryApp e op e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
   case find op ρ of
      Just { u: V.Op φ } -> { t: T.BinaryApp t op t', v: toValues (opFun φ) v v' }
      Just _ -> error absurd
      Nothing -> error $ "operator " <> op <> " not found"
eval ρ (Expr _ (E.Let x e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval (ρ :+: x ↦ v) e'
   in { t: T.Let x t t', v: v' }
eval ρ (Expr _ (E.Match e σ)) =
   let { t, v } = eval ρ e
   in case match v σ of
      Nothing -> error "Pattern mismatch"
      Just (T3 ρ' e' ξ) ->
         let { t: t', v: v' } = eval (ρ <> ρ') e'
         in { t: T.Match t ξ t', v: v' }
