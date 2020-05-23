module Eval where

import Prelude hiding (absurd)
import Data.Maybe (Maybe(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Expl(..)) as T
import Expl (Expl, Match(..), Match2(..))
import Expr (Elim(..), Elim2(..), Expr, RawExpr(..), T3(..))
import Primitive (opFun)
import Util (absurd, error)
import Val (Env, Val, toValues, val)
import Val (RawVal(..)) as V

match :: Val -> Elim -> Maybe (T3 Env Expr Match)
match v (ElimVar { x, e }) = Just $ T3 (ε :+: x ↦ v) e (MatchVar x)
match { u: V.True } (ElimBool { true: e1, false: _ }) = Just $ T3 ε e1 MatchTrue
match { u: V.False } (ElimBool { true: _, false: e2 }) = Just $ T3 ε e2 MatchFalse
match { u: V.Pair v v' } (ElimPair { x, y, e }) = Just $ T3 (ε :+: x ↦ v :+: y ↦ v') e (MatchPair x y)
match { u: V.Nil } (ElimList { nil: e, cons: _ }) = Just $ T3 ε e MatchNil
match { u : V.Cons v v' } (ElimList { nil: _, cons: { x, y, e } }) =
   Just $ T3 (ε :+: x ↦ v :+: y ↦ v') e (MatchCons x y)
match _ _ = Nothing

match2 :: forall k . Val -> Elim2 k -> Maybe (T3 Env k (Match2 k))
match2 v (ElimVar2 x κ) = Just $ T3 (ε :+: x ↦ v) κ (MatchVar2 x)
match2 { u: V.True } (ElimBool2 { true: κ, false: κ' }) = Just $ T3 ε κ (MatchTrue2 κ')
match2 { u: V.False } (ElimBool2 { true: κ, false: κ' }) = Just $ T3 ε κ' (MatchFalse2 κ)
match2 { u: V.Pair v v' } (ElimPair2 σ) = do
   T3 ρ1 τ ξ <- match2 v σ
   T3 ρ2 κ ξ' <- match2 v' τ
   pure $ T3 (ρ1 <> ρ2) κ (MatchPair2 ξ ξ')
match2 { u: V.Nil } (ElimList2 { nil: κ, cons: σ }) = Just $ T3 ε κ (MatchNil2 σ)
match2 _ _ = Nothing

type ExplVal = { t :: Expl, v :: Val }

eval :: Env -> Expr -> ExplVal
eval ρ { r: Var x } =
   case find x ρ of
      Just v -> { t: T.Var x, v }
      _ -> error $ "variable " <> x <> " not found"
eval ρ { r: Op op } =
   case find op ρ of
      Just v -> { t: T.Op op, v }
      _ -> error $ "operator " <> op <> " not found"
eval ρ { r: True } = { t: T.True, v: val V.True }
eval ρ { r: False } = { t: T.False, v: val V.False }
eval ρ { r: Int n } = { t: T.Int n, v: val $ V.Int n }
eval ρ { r: Pair e e' } =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e'
   in  { t: T.Pair t t', v: val $ V.Pair v v' }
eval ρ { r: Nil } = { t: T.Nil, v: val V.Nil }
eval ρ { r: Cons e e' } =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e'
   in { t: T.Cons t t', v: val $ V.Cons v v' }
eval ρ { r: Letrec f σ e } =
   let { t, v } = eval (ρ :+: f ↦ (val $ V.Closure ρ f σ)) e
   in { t: T.Letrec f (T.Fun ρ σ) t, v }
eval ρ { r: App e e' } =
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
eval ρ { r : BinaryApp e op e' } =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
   case find op ρ of
      Just { u: V.Op φ } -> { t: T.BinaryApp t op t', v: toValues (opFun φ) v v' }
      Just _ -> error absurd
      Nothing -> error $ "operator " <> op <> " not found"
eval ρ { r : Let x e e' } =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval (ρ :+: x ↦ v) e'
   in { t: T.Let x t t', v: v' }
eval ρ { r : Match e σ } =
   let { t, v } = eval ρ e
   in case match v σ of
      Nothing -> error "Pattern mismatch"
      Just (T3 ρ' e' ξ) ->
         let { t: t', v: v' } = eval (ρ <> ρ') e'
         in { t: T.Match t ξ t', v: v' }
