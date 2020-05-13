module Eval where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), conc, find)
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Expr
import Util (error)
import Val (Env, Val, val)
import Val (RawVal(..)) as V

match :: Val -> Elim -> Maybe (T3 Env Expr Match)
-- var
match v (ElimVar { x, e }) = Just $ T3 (Empty :+: Bind x v) e (MatchVar x)
-- true
match { u: V.True } (ElimBool { btrue: e1, bfalse: _ }) = Just $ T3 Empty e1 MatchTrue
-- false
match { u: V.False } (ElimBool { btrue: _, bfalse: e2 }) = Just $ T3 Empty e2 MatchFalse
-- pair
match { u: V.Pair v v' } (ElimPair { x, y, e }) = Just $ T3 (Empty :+: Bind x v :+: Bind y v') e (MatchPair x y)
-- nil
match { u: V.Nil } (ElimList { bnil: e, bcons: _ }) = Just $ T3 Empty e MatchNil
-- cons
match { u : V.Cons v v' } (ElimList { bnil: _, bcons: { x, y, e } }) =
   Just $ T3 (Empty :+: Bind x v :+: Bind y v') e (MatchCons x y)
-- failure
match _ _ = Nothing

type ExplVal = { t :: Expl, v :: Val }

eval :: Partial => Env -> Expr -> ExplVal
-- var
eval ρ { r: Var x } =
   case find x ρ of
      Just v -> { t: T.Var x, v }
      _ -> error $ "variable " <> x <> " not found"
-- true
eval ρ { r: True } = { t: T.True, v: val V.True }
-- false
eval ρ { r: False } = { t: T.False, v: val V.False }
-- int
eval ρ { r: Int n } = { t: T.Int n, v: val $ V.Int n }
-- pair
eval ρ { r: Pair e1 e2 } =
   let { t: t1, v: v1 } = eval ρ e1
       { t: t2, v: v2 } = eval ρ e2
   in  { t: T.Pair t1 t2, v: val $ V.Pair v1 v2 }
-- nil
eval ρ { r: Nil } = { t: T.Nil, v: val V.Nil }
-- cons
eval ρ { r: Cons e e' } =
   let { t: t1, v: v1 } = eval ρ e
       { t: t2, v: v2 } = eval ρ e'
   in  { t: T.Cons t1 t2, v: val $ V.Cons v1 v2 }
-- letrec
eval ρ { r: Letrec f σ e } =
   let { t, v } = eval (ρ :+: Bind f (val $ V.Closure ρ f σ)) e
   in { t: T.Letrec f (T.Fun ρ σ) t, v }
-- apply
eval ρ { r: App e e' } =
   case eval ρ e of
      { t, v: { u: V.Closure ρ' fun σ } } ->
         let { t: t', v } = eval ρ e'
         in case match v σ of
            Just (T3 ρ'' e'' ξ) ->
               let { t: u, v: v' } = eval (conc ρ' ρ'' :+: Bind fun v) e''
               in { t: T.App t t' ξ u, v: v' }
            Nothing -> error "Match not found"
      _ -> error "Expected closure"
-- binary app
eval ρ { r : BinaryApp op e1 e2 } =
   let { t: t1, v: v1 } = eval ρ e1
       { t: t2, v: v2 } = eval ρ e2
   in case v1, v2 of
      { u: V.Int n1 }, {u : V.Int n2 } -> { t: T.BinaryApp op t1 t2, v: val $ V.Int (n1 + n2) }
      _, _ -> error "Arithmetic type error: e1 or/and e2 do not evaluate to ints"
-- let
eval ρ { r : Let x e1 e2 } =
   let { t: t1, v: v1 } = eval ρ e1
       { t: t2, v: v2 }  = eval (ρ :+: Bind x v1) e2
   in {t: T.Let x t1 t2, v: v2 }
-- match
eval ρ { r : Match e σ } =
   let { t: t1, v: v1 } = eval ρ e
   in case match v1 σ of
      Nothing -> error "Match not found"
      Just (T3 ρ' e' ξ) ->
         let { t: t2, v: v2 } = eval (conc ρ ρ') e'
         in { t: T.Match t1 ξ t2, v: v2 }
