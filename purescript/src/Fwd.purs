module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), (:++:), find)
import Expr
import Expl (Expl(..), Match(..))
import Val (Env, Val)
import Val (Val(..)) as V

fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Availability)
-- var
fwd_match v (ElimVar { x, tx, e }) (MatchVar mx) = Just $ T3 (Empty :+: Bind x v) e Top
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
fwd_match (V.PairSel u v) (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- pair-bot
fwd_match V.Bot (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bottom
-- pair
fwd_match (V.Pair u v) (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bottom
-- nil-sel
fwd_match V.NilSel (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Top
-- nil
fwd_match V.Nil (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- nil-bot
fwd_match V.Bot (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- cons-sel
fwd_match (V.ConsSel u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- cons-bot
fwd_match V.Bot (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
   let ρ' = Empty :+: Bind x V.Bot :+: Bind y V.Bot
   in  Just $ T3 ρ' e Bottom
-- cons
fwd_match (V.Cons u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
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
fwd ρ TrueSel ExplTrue Top = V.TrueSel
-- true-bot
fwd ρ TrueSel ExplTrue Bottom = V.Bot
-- true
fwd ρ True ExplTrue _ = V.Bot
-- false-sel
fwd ρ FalseSel ExplFalse Top = V.FalseSel
-- false-bot
fwd ρ FalseSel ExplFalse Bottom = V.Bot
-- false-bot
fwd ρ False ExplFalse _ = V.Bot
-- int-sel
fwd ρ (IntSel n) (ExplInt tn) Top = V.IntSel n
-- int-bot
fwd ρ (IntSel n) (ExplInt tn) Bottom = V.Bot
-- int
fwd ρ (Int n) (ExplInt tn) _ = V.Bot
-- pair-sel
fwd ρ (PairSel e1 e2) (ExplPair te1 te2) Top = V.PairSel (fwd ρ e1 te1 Top) (fwd ρ e2 te2 Top)
-- pair
fwd ρ (Pair e1 e2) (ExplPair te1 te2) α = V.Pair (fwd ρ e1 te1 α) (fwd ρ e2 te2 α)
-- nil-sel
fwd ρ NilSel ExplNil Top = V.NilSel
-- nil-bot
fwd ρ NilSel ExplNil Bottom = V.Bot
-- nil
fwd ρ Nil ExplNil _ = V.Bot
-- cons-sel
fwd ρ (ConsSel e es) (ExplCons te tes) Top = V.ConsSel (fwd ρ e te Top) (fwd ρ es tes Top)
-- cons-sel
fwd ρ (Cons e es) (ExplCons te tes) α = V.Cons (fwd ρ e te α) (fwd ρ es tes α)
-- letrec (fun)
fwd ρ (Letrec f σ e) (ExplLetrec x tσ te) α = fwd (ρ :+: Bind f (V.Closure ρ f σ)) e te α
-- apply
fwd ρ (App e e') (ExplApp te te' m tu) α =
   case fwd ρ e te α  of
      V.Closure ρ' f σ ->
         case fwd_match (fwd ρ e' te' α) σ m of
            Just (T3 ρ'' e''  α') -> fwd (ρ' :++: ρ'' :+: Bind f (V.Closure ρ' f σ)) e'' tu α'
            Nothing -> V.Failure "Match not found"
      _  -> V.Failure "Impossible"
-- add-bot
fwd ρ (Add e1 e2) (ExplAdd te1 te2) Bottom = V.Bot
fwd ρ (Add e1 e2) (ExplAdd te1 te2) Top =
   let v1 = fwd ρ e1 te1 Top
       v2 = fwd ρ e2 te2 Top
   in case v1, v2 of
      -- add
      (V.Int n1), (V.Int n2) -> V.Int (n1 + n2)
      -- add-bot-1
      V.Bot, _ -> V.Bot
      -- add-bot-2
      _, V.Bot -> V.Bot
      _, _ -> V.Failure "Impossible"
-- let
fwd ρ (Let x e1 e2) (ExplLet tx te1 te2) α =
   let v1  = fwd ρ e1 te1 α
       ρ'  = (ρ :+: Bind x v1)
   in  fwd ρ' e2 te2 α
-- match (no rule in paper)
fwd ρ (Match e σ) (ExplMatch te m tu) α =
   case fwd_match (fwd ρ e te α) σ m of
      Nothing -> V.Failure "Impossible"
      Just (T3 ρ' e' α') -> fwd (ρ :++: ρ') e' tu α'
