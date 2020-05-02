module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), (:++:), find)
import Expr
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Val (Env, Val)
import Val (Val(..)) as V

fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Availability)
-- var
fwd_match v (ElimVar { x, e }) (MatchVar _) = Just $ T3 (Empty :+: Bind x v) e Top
-- true-sel
fwd_match V.TrueSel (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Top
-- true
fwd_match V.True (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bottom
-- true-bot
fwd_match V.Bot (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bottom
-- false-sel
fwd_match V.FalseSel (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Top
-- false
fwd_match V.False (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bottom
-- false-bot
fwd_match V.Bot (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bottom
-- pair-sel
fwd_match (V.PairSel u v) (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- pair-bot
fwd_match V.Bot (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bottom
-- pair
fwd_match (V.Pair u v) (ElimPair { x, y, e }) (MatchPair _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bottom
-- nil-sel
fwd_match V.NilSel (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Top
-- nil
fwd_match V.Nil (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- nil-bot
fwd_match V.Bot (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- cons-sel
fwd_match (V.ConsSel u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- cons-bot
fwd_match V.Bot (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bottom
-- cons
fwd_match (V.Cons u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons _ _) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bottom
-- failure
fwd_match _ _ _ =  Nothing


-- TODO: remove Partial typeclass.
fwd :: Partial => Env -> Expr -> Expl -> Availability -> Val
-- var
fwd ρ (Var x) t α =
   case find x ρ of
      Just val -> val
      _        -> V.Failure ("variable " <> x <> " not found")
-- true-sel
fwd ρ TrueSel T.True Top = V.TrueSel
-- true-bot
fwd ρ TrueSel T.True Bottom = V.Bot
-- true
fwd ρ True T.True _ = V.Bot
-- false-sel
fwd ρ FalseSel T.False Top = V.FalseSel
-- false-bot
fwd ρ FalseSel T.False Bottom = V.Bot
-- false-bot
fwd ρ False T.False _ = V.Bot
-- int-sel
fwd ρ (IntSel n) (T.Int _) Top = V.IntSel n
-- int-bot
fwd ρ (IntSel n) (T.Int _) Bottom = V.Bot
-- int
fwd ρ (Int n) (T.Int _) _ = V.Bot
-- pair-sel
fwd ρ (PairSel e1 e2) (T.Pair t1 t2) Top = V.PairSel (fwd ρ e1 t1 Top) (fwd ρ e2 t2 Top)
-- pair
fwd ρ (Pair e1 e2) (T.Pair t1 t2) α = V.Pair (fwd ρ e1 t1 α) (fwd ρ e2 t2 α)
-- nil-sel
fwd ρ NilSel T.Nil Top = V.NilSel
-- nil-bot
fwd ρ NilSel T.Nil Bottom = V.Bot
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
-- add-bot
fwd ρ (Add e1 e2) (T.Add t1 t2) Bottom = V.Bot
fwd ρ (Add e1 e2) (T.Add t1 t2) Top =
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
