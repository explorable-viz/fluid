module Eval where

import Prelude hiding (absurd)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Elim(..), Expr(..), RawExpr(..), T3(..))
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
   T3 ρ κ ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ) κ (MatchPair ξ ξ')
match { u: V.Nil } (ElimList { nil: κ, cons: σ }) = Just $ T3 ε κ (MatchNil σ)
match { u : V.Cons v v' } (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ κ' ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ) κ' (MatchCons { nil: κ, cons: Tuple ξ ξ' })
match _ _ = Nothing

type ExplVal = { t :: Expl, v :: Val }

eval :: Env -> Expr -> ExplVal
eval ρ (Expr _ (Var x)) =
   case find x ρ of
      Just v -> { t: T.Var x, v }
      _ -> error $ "variable " <> x <> " not found"
eval ρ (Expr _ (Op op)) =
   case find op ρ of
      Just v -> { t: T.Op op, v }
      _ -> error $ "operator " <> op <> " not found"
eval ρ (Expr _ True) = { t: T.True, v: val V.True }
eval ρ (Expr _ False) = { t: T.False, v: val V.False }
eval ρ (Expr _ (Int n)) = { t: T.Int n, v: val $ V.Int n }
eval ρ (Expr _ (Pair e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e'
   in { t: T.Pair t t', v: val $ V.Pair v v' }
eval ρ (Expr _ Nil) = { t: T.Nil, v: val V.Nil }
eval ρ (Expr _ (Cons e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e'
   in { t: T.Cons t t', v: val $ V.Cons v v' }
eval ρ (Expr _ (Letrec f σ e)) =
   let { t, v } = eval (ρ :+: f ↦ (val $ V.Closure ρ f σ)) e
   in { t: T.Letrec f (T.Fun ρ σ) t, v }
eval ρ (Expr _ (App e e')) =
   case eval ρ e, eval ρ e' of
      { t, v: { u: V.Closure ρ' f σ } }, { t: t', v } ->
         case match v σ of
            Just (T3 ρ'' e'' ξ) ->
               let { t: u, v: v' } = eval ((ρ' <> ρ'') :+: f ↦ v) e''
               in { t: T.App t t' ξ u, v: v' }
            Nothing -> error "Pattern mismatch"
      { t, v: { u: V.Op op } }, { t: t', v } ->
         { t: T.AppOp t t', v: val $ V.PartialApp op v }
      { t, v: { u: V.PartialApp op v } }, { t: t', v: v' } ->
         { t: T.AppOp t t', v: toValues (opFun op) v v' }
      _, _ -> error "Expected closure or operator"
eval ρ (Expr _ (BinaryApp e op e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
   case find op ρ of
      Just { u: V.Op φ } -> { t: T.BinaryApp t op t', v: toValues (opFun φ) v v' }
      Just _ -> error absurd
      Nothing -> error $ "operator " <> op <> " not found"
eval ρ (Expr _ (Let x e e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval (ρ :+: x ↦ v) e'
   in { t: T.Let x t t', v: v' }
eval ρ (Expr _ (Match e σ)) =
   let { t, v } = eval ρ e
   in case match v σ of
      Nothing -> error "Pattern mismatch"
      Just (T3 ρ' e' ξ) ->
         let { t: t', v: v' } = eval (ρ <> ρ') e'
         in { t: T.Match t ξ t', v: v' }
