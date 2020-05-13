module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), (:++:), find)
import Expr
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Selected (Selected, (∧))
import Util (absurd)
import Val (Env, Val)
import Val (Val, RawVal(..)) as V


fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Selected)
-- var
fwd_match v (ElimVar { x, e }) (MatchVar _) = Just $ T3 (Empty :+: Bind x v) e Top
-- true-sel
fwd_match V.TrueSel (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Top
-- true
fwd_match V.True (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bot
-- true-bot
fwd_match V.Bot (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bot
-- false-sel
fwd_match V.FalseSel (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Top
-- false
fwd_match V.False (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bot
-- false-bot
fwd_match V.Bot (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bot
-- pair-sel
fwd_match (V.PairSel u v) (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- pair-bot
fwd_match V.Bot (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bot
-- pair
fwd_match (V.Pair u v) (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bot
-- nil-sel
fwd_match V.NilSel (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Top
-- nil
fwd_match V.Nil (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bot
-- nil-bot
fwd_match V.Bot (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bot
-- cons-sel
fwd_match (V.ConsSel u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- cons-bot
fwd_match V.Bot (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bot
-- cons
fwd_match (V.Cons u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bot
-- failure
fwd_match _ _ _ =  Nothing

-- TODO: remove Partial typeclass.
fwd :: Partial => Env -> Expr -> Expl -> Selected -> Val
fwd ρ (Var x) t α =
   case find x ρ of
      Just val -> val
      _        -> V.Failure ("variable " <> x <> " not found")
fwd ρ { α, r: True } _ α' = { α: α ∧ α', u: True }
fwd ρ { α, r: False } _ α' = { α: α ∧ α', u: False }
fwd ρ { α, r: Int n } _ α' = { α: α ∧ α', u: Int n }
fwd ρ { α, r: Pair e1 e2 } (T.Pair t1 t2) α' = { α: α ∧ α', u: V.Pair (fwd ρ e1 t1 α') (fwd ρ e2 t2 α') }
fwd ρ { α, r: Nil} _ α' = { α: α ∧ α', u: V.Nil }
fwd ρ { α, r: Cons e e' } (T.Cons t t') α' = { α: α ∧ α', u: V.Cons (fwd ρ e t α') (fwd ρ e' t' α') }
-- letrec (fun)
fwd ρ (Letrec f σ e) (T.Letrec _ _ t) α = fwd (ρ :+: Bind f (V.Closure ρ f σ)) e t α
fwd ρ (App e e') (T.App t t' ξ t'') α =
   case fwd ρ e t α  of
      { α: α', u: V.Closure ρ' f σ } ->
         case fwd_match (fwd ρ e' t' α) σ ξ of
            Just (T3 ρ'' e'' α'') ->
               let ρ_f = ρ' :++: ρ'' :+: Bind f { α: α', u: (V.Closure ρ' f σ) } in fwd ρ_f e'' t'' (α' ∧ α'')
            Nothing -> absurd
      _  -> absurd
-- binary-app-bot
fwd ρ (BinaryApp op e1 e2) (T.BinaryApp _ t1 t2) Bot = V.Bot
-- binary-app
fwd ρ (BinaryApp op e1 e2) (T.BinaryApp _ t1 t2) α =
   case fwd ρ e1 t1 α, fwd ρ e2 t2 α of
   { α: α', u: V.Int n1 }, { α: a'', u: V.Int n2 } -> { α: α ∧ α' ∧ α'', u: V.Int (n1 + n2) }
   _, _ -> absurd
fwd ρ (Let x e1 e2) (T.Let _ t1 t2) α =
   fwd (ρ :+: Bind x (fwd ρ e1 t1 α)) e2 t2 α
fwd ρ (Match e σ) (T.Match t ξ t') α =
   case fwd_match (fwd ρ e t α) σ ξ of
      Just (T3 ρ' e' α') -> fwd (ρ :++: ρ') e' t' α'
      Nothing -> absurd
