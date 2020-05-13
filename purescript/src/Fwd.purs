module Fwd where

import Prelude (($), (<>))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), find)
import Expr (Elim(..), Expr, RawExpr(..), T3(..))
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Selected (Selected(..), (∧))
import Util (absurd)
import Val (Env, Val)
import Val (RawVal(..)) as V


fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Selected)
-- var
fwd_match v (ElimVar { x, e }) (MatchVar _) = Just $ T3 (Empty :+: Bind x v) e Top
-- true
fwd_match { α, u: V.True } (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e α
-- false
fwd_match { α, u: V.False } (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e α
-- pair
fwd_match { α, u: V.Pair u v } (ElimPair { x, y, e }) (MatchPair _ _) =
   Just $ T3 (Empty :+: Bind x u :+: Bind y v) e α
-- nil
fwd_match { α, u: V.Nil } (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e α
-- cons
fwd_match { α, u: V.Cons u v } (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   Just $ T3 (Empty :+: Bind x u :+: Bind y v) e Top
-- failure
fwd_match _ _ _ =  Nothing

fwd :: Env -> Expr -> Expl -> Selected -> Val
-- var
fwd ρ { r: Var x } t _ =
   case find x ρ of
      Just val -> val
      _ -> absurd
-- true
fwd ρ { α, r: True } _ α' = { α: α ∧ α', u: V.True }
-- false
fwd ρ { α, r: False } _ α' = { α: α ∧ α', u: V.False }
-- int
fwd ρ { α, r: Int n } _ α' = { α: α ∧ α', u: V.Int n }
-- pair
fwd ρ { α, r: Pair e1 e2 } (T.Pair t1 t2) α' = { α: α ∧ α', u: V.Pair (fwd ρ e1 t1 α') (fwd ρ e2 t2 α') }
-- nil
fwd ρ { α, r: Nil} _ α' = { α: α ∧ α', u: V.Nil }
-- cons
fwd ρ { α, r: Cons e e' } (T.Cons t t') α' = { α: α ∧ α', u: V.Cons (fwd ρ e t α') (fwd ρ e' t' α') }
-- letrec
fwd ρ { r: Letrec f σ e } (T.Letrec _ _ t) α = fwd (ρ :+: Bind f { α, u: V.Closure ρ f σ }) e t α
-- app
fwd ρ { r: App e e' } (T.App t t' ξ t'') α =
   case fwd ρ e t α  of
      { α: α', u: V.Closure ρ' f σ } ->
         case fwd_match (fwd ρ e' t' α) σ ξ of
            Just (T3 ρ'' e'' α'') ->
               let ρ_f = (ρ' <> ρ'') :+: Bind f { α: α', u: (V.Closure ρ' f σ) } in fwd ρ_f e'' t'' (α' ∧ α'')
            Nothing -> absurd
      _  -> absurd
-- binary app
fwd ρ { r: BinaryApp op e1 e2 } (T.BinaryApp _ t1 t2) α =
   case fwd ρ e1 t1 α, fwd ρ e2 t2 α of
   { α: α', u: V.Int n1 }, { α: α'', u: V.Int n2 } -> { α: α ∧ α' ∧ α'', u: V.Int (n1 + n2) }
   _, _ -> absurd
-- let
fwd ρ { r: Let x e1 e2 } (T.Let _ t1 t2) α =
   fwd (ρ :+: Bind x (fwd ρ e1 t1 α)) e2 t2 α
-- match
fwd ρ { r: Match e σ } (T.Match t ξ t') α =
   case fwd_match (fwd ρ e t α) σ ξ of
      Just (T3 ρ' e' α') -> fwd (ρ <> ρ') e' t' α'
      Nothing -> absurd
-- trace/expression mismatch
fwd _ _ _ _ = absurd
