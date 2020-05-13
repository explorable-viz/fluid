module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), (:++:), find)
import Expr
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Val (Env, Val)
import Val (Val, RawVal(..)) as V
import Selected (Selected, (∧))


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
-- pair-sel
fwd ρ { α, r: Pair e1 e2 } (T.Pair t1 t2) α' = { α: α ∧ α', u: V.Pair (fwd ρ e1 t1 Top) (fwd ρ e2 t2 Top) }
-- nil-sel
fwd ρ NilSel T.Nil Top = V.NilSel
-- nil-bot
fwd ρ NilSel T.Nil Bot = V.Bot
-- nil
fwd ρ Nil T.Nil _ = V.Bot
-- cons-sel
fwd ρ (ConsSel e e') (T.Cons t t') Top = V.ConsSel (fwd ρ e t Top) (fwd ρ e' t' Top)
-- cons-sel
fwd ρ (Cons e e') (T.Cons t t') α = V.Cons (fwd ρ e t α) (fwd ρ e' t' α)
-- letrec (fun)
fwd ρ (Letrec f σ e) (T.Letrec _ _ t) α = fwd (ρ :+: Bind f (V.Closure ρ f σ)) e t α
-- apply
fwd ρ (App e e') (T.App t t' ξ t'') α =
   case fwd ρ e t α  of
      V.Closure ρ' f σ ->
         case fwd_match (fwd ρ e' t' α) σ ξ of
            Just (T3 ρ'' e''  α') -> fwd (ρ' :++: ρ'' :+: Bind f (V.Closure ρ' f σ)) e'' t'' α'
            Nothing -> V.Failure "Match not found"
      _  -> V.Failure "Impossible"
-- binary-app-bot
fwd ρ (BinaryApp op e1 e2) (T.BinaryApp _ t1 t2) Bot = V.Bot
-- binary-app
fwd ρ (BinaryApp op e1 e2) (T.BinaryApp _ t1 t2) Top =
   let v1 = fwd ρ e1 t1 Top
       v2 = fwd ρ e2 t2 Top
   in case v1, v2 of
      -- add
      (V.Int n1), (V.Int n2) -> V.Int (n1 + n2)
      -- add-bot-1
      V.Bot, _ -> V.Bot
      -- add-bot-2
      _, V.Bot -> V.Bot
      _, _ -> V.Failure "Impossible"
-- let
fwd ρ (Let x e1 e2) (T.Let _ t1 t2) α =
   let v1  = fwd ρ e1 t1 α
       ρ'  = (ρ :+: Bind x v1)
   in  fwd ρ' e2 t2 α
-- match (no rule in paper)
fwd ρ (Match e σ) (T.Match t ξ t') α =
   case fwd_match (fwd ρ e t α) σ ξ of
      Nothing -> V.Failure "Impossible"
      Just (T3 ρ' e' α') -> fwd (ρ :++: ρ') e' t' α'
