module Eval where

import Prelude hiding (absurd, apply)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Def(..), Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Elim(..), Expr(..), Module(..), RecDef(..), RecDefs)
import Expr (Def(..), RawExpr(..)) as E
import Pretty (pretty, render)
import Primitive (apply)
import Util (T3(..), absurd, error)
import Val (Env, Val(..), val)
import Val (RawVal(..)) as V

match :: forall k . Val -> Elim k -> Maybe (T3 Env k (Match k))
match v (ElimVar x κ) = Just $ T3 (ε :+: x ↦ v) κ (MatchVar x)
match (Val _ V.True) (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ (MatchTrue κ')
match (Val _ V.False) (ElimBool { true: κ, false: κ' }) = Just $ T3 ε κ' (MatchFalse κ)
match (Val _ (V.Pair v v')) (ElimPair σ) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ2 κ ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ2) κ (MatchPair ξ ξ')
match (Val _ V.Nil) (ElimList { nil: κ, cons: σ }) = Just $ T3 ε κ (MatchNil σ)
match (Val _ (V.Cons v v')) (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ κ' ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ) κ' (MatchCons { nil: κ, cons: Tuple ξ ξ' })
match _ _ = Nothing

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> RecDefs -> RecDefs -> Env
closeDefs _ _ Nil = ε
closeDefs ρ δ0 (RecDef f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ σ)

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
eval ρ (Expr _ (E.LetRec δ e)) =
   let ρ' = closeDefs ρ δ δ
       { t, v } = eval (ρ <> ρ') e in
   { t: T.LetRec δ t, v }
eval ρ (Expr _ (E.Lambda σ)) =
   { t: T.Lambda σ, v: val $ V.Closure ρ Nil σ }
eval ρ (Expr _ (E.App e e')) =
   case eval ρ e, eval ρ e' of
      { t, v: (Val _ (V.Closure ρ1 δ σ)) }, { t: t', v } ->
         let ρ2 = closeDefs ρ1 δ δ in
         case match v σ of
            Just (T3 ρ3 e'' ξ) ->
               let { t: u, v: v' } = eval (ρ1 <> ρ2 <> ρ3) e''
               in { t: T.App t t' ξ u, v: v' }
            Nothing -> error $ "Pattern mismatch for " <> render (pretty v)
      { t, v: (Val _ (V.Op op)) }, { t: t', v } ->
         { t: T.AppOp t t', v: val $ V.PartialApp op v }
      { t, v: (Val _ (V.PartialApp φ v)) }, { t: t', v: v' } ->
         { t: T.AppOp t t', v: apply φ v v' }
      _, _ -> error "Expected closure or operator"
eval ρ (Expr _ (E.BinaryApp e op e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval ρ e' in
   case find op ρ of
      Just (Val _ (V.Op φ)) -> { t: T.BinaryApp t op t', v: apply φ v v' }
      Just _ -> error absurd
      Nothing -> error $ "operator " <> op <> " not found"
eval ρ (Expr _ (E.Let (E.Def x e) e')) =
   let { t, v } = eval ρ e
       { t: t', v: v' } = eval (ρ :+: x ↦ v) e'
   in { t: T.Let (T.Def x t) t', v: v' }
eval ρ (Expr _ (E.MatchAs e σ)) =
   let { t, v } = eval ρ e
   in case match v σ of
      Nothing -> error $ "Pattern mismatch for " <> render (pretty v)
      Just (T3 ρ' e' ξ) ->
         let { t: t', v: v' } = eval (ρ <> ρ') e'
         in { t: T.MatchAs t ξ t', v: v' }

defs :: Env -> Module -> Env
defs ρ (Module Nil) = ρ
defs ρ (Module ((E.Def x e) : ds)) =
   defs (ρ :+: x ↦ (eval ρ e).v) (Module ds)
