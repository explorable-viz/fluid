module Eval where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Def(..), Def2(..), Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Elim(..), Expr(..), Module(..), RecDef(..), RecDefs)
import Expr (Def(..), Def2(..), RawExpr(..)) as E
import Pretty (pretty, render)
import Primitive (applyBinary, applyUnary)
import Util (T3(..), absurd, error)
import Val (Env, UnaryOp(..), Val(..), val)
import Val (RawVal(..)) as V

type Error = String

match :: forall k . Val -> Elim k -> Either Error (T3 Env k (Match k))
match v (ElimVar x κ) =
   pure $ T3 (ε :+: x ↦ v) κ (MatchVar x)
match (Val _ V.True) (ElimBool { true: κ, false: κ' }) =
   pure $ T3 ε κ (MatchTrue κ')
match (Val _ V.False) (ElimBool { true: κ, false: κ' }) =
   pure $ T3 ε κ' (MatchFalse κ)
match (Val _ (V.Pair v v')) (ElimPair σ) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ2 κ ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ2) κ (MatchPair ξ ξ')
match (Val _ V.Nil) (ElimList { nil: κ, cons: σ }) =
   pure $ T3 ε κ (MatchNil σ)
match (Val _ (V.Cons v v')) (ElimList { nil: κ, cons: σ }) = do
   T3 ρ1 τ ξ <- match v σ
   T3 ρ κ' ξ' <- match v' τ
   pure $ T3 (ρ1 <> ρ) κ' (MatchCons { nil: κ, cons: Tuple ξ ξ' })
match v _ =
   Left $ "Pattern mismatch for " <> render (pretty v)

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> RecDefs -> RecDefs -> Env
closeDefs _ _ Nil = ε
closeDefs ρ δ0 (RecDef f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ0 σ)

type ExplVal = { t :: Expl, v :: Val }

eval :: Env -> Expr -> Either Error ExplVal
eval ρ (Expr _ (E.Var x)) =
   find x ρ <#> \v -> { t: T.Var x, v }
eval ρ (Expr _ (E.Op op)) =
   find op ρ <#> \v -> { t: T.Op op, v }
eval ρ (Expr _ E.True) =
   pure { t: T.True, v: val V.True }
eval ρ (Expr _ E.False) =
   pure { t: T.False, v: val V.False }
eval ρ (Expr _ (E.Int n)) =
   pure { t: T.Int n, v: val $ V.Int n }
eval ρ (Expr _ (E.Str str)) =
   pure { t: T.Str str, v: val $ V.Str str }
eval ρ (Expr _ (E.Pair e e')) = do
   { t, v } <- eval ρ e
   { t: t', v: v' } <- eval ρ e'
   pure { t: T.Pair t t', v: val $ V.Pair v v' }
eval ρ (Expr _ E.Nil) =
   pure { t: T.Nil, v: val V.Nil }
eval ρ (Expr _ (E.Cons e e')) = do
   { t, v } <- eval ρ e
   { t: t', v: v' } <- eval ρ e'
   pure { t: T.Cons t t', v: val $ V.Cons v v' }
eval ρ (Expr _ (E.LetRec δ e)) = do
   let ρ' = closeDefs ρ δ δ
   { t, v } <- eval (ρ <> ρ') e
   pure { t: T.LetRec δ t, v }
eval ρ (Expr _ (E.Lambda σ)) =
   pure { t: T.Lambda σ, v: val $ V.Closure ρ Nil σ }
eval ρ (Expr _ (E.App e e')) = do
   { t, v } <- eval ρ e
   { t: t', v: v' } <- eval ρ e'
   case v of
      Val _ (V.Closure ρ1 δ σ) -> do
         let ρ2 = closeDefs ρ1 δ δ
         T3 ρ3 e'' ξ <- match v' σ
         { t: u, v: v'' } <- eval (ρ1 <> ρ2 <> ρ3) e''
         pure { t: T.App t t' ξ u, v: v'' }
      Val _ (V.Unary φ) ->
         pure { t: T.AppOp t t', v: applyUnary φ v' }
      Val _ (V.Binary φ) ->
         pure { t: T.AppOp t t', v: val $ V.Unary (PartialApp φ v') }
      _ -> Left "Expected closure or operator"
eval ρ (Expr _ (E.BinaryApp e op e')) = do
   { t, v } <- eval ρ e
   { t: t', v: v' } <- eval ρ e'
   Val _ u <- find op ρ
   case u of
      V.Binary φ -> pure { t: T.BinaryApp t op t', v: v `applyBinary φ` v' }
      _ -> error absurd
eval ρ (Expr _ (E.Let (E.Def x e) e')) = do
   { t, v } <- eval ρ e
   { t: t', v: v' } <- eval (ρ :+: x ↦ v) e'
   pure { t: T.Let (T.Def x t) t', v: v' }
eval ρ (Expr _ (E.Let2 (E.Def2 σ e) e')) = do
   { t, v } <- eval ρ e
   T3 ρ' _ ξ <- match v σ
   { t: t', v: v' } <- eval (ρ <> ρ') e'
   pure { t: T.Let2 (T.Def2 ξ t) t', v: v' }
eval ρ (Expr _ (E.MatchAs e σ)) = do
   { t, v } <- eval ρ e
   T3 ρ' e' ξ <- match v σ
   { t: t', v: v' } <- eval (ρ <> ρ') e'
   pure { t: T.MatchAs t ξ t', v: v' }

defs :: Env -> Module -> Either Error Env
defs ρ (Module Nil) = pure ρ
defs ρ (Module (Left (E.Def x e) : ds)) = do
   { v } <- eval ρ e
   defs (ρ :+: x ↦ v) (Module ds)
defs ρ (Module (Right δ : ds)) =
   defs (ρ <> closeDefs ρ δ δ) (Module ds)
