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
fwd_match V.ValTrueSel (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Top
-- true
fwd_match V.ValTrue (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bottom
-- true-bot
fwd_match V.ValBottom (ElimBool { btrue: e, bfalse: _ }) MatchTrue = Just $ T3 Empty e Bottom
-- false-sel
fwd_match V.ValFalseSel (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Top
-- false
fwd_match V.ValFalse (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bottom
-- false-bot
fwd_match V.ValBottom (ElimBool { btrue: _, bfalse: e }) MatchFalse = Just $ T3 Empty e Bottom
-- pair-sel
fwd_match (V.ValPairSel u v) (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- pair-bot
fwd_match V.ValBottom (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x V.ValBottom :+: Bind y V.ValBottom
   in  Just $ T3 ρ' e Bottom
-- pair
fwd_match (V.ValPair u v) (ElimPair { x, y, e }) (MatchPair mx my) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bottom
-- nil-sel
fwd_match V.ValNilSel (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Top
-- nil
fwd_match V.ValNil (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- nil-bot
fwd_match V.ValBottom (ElimList { bnil: e, bcons: _ }) MatchNil = Just $ T3 Empty e Bottom
-- cons-sel
fwd_match (V.ValConsSel u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Top
-- cons-bot
fwd_match V.ValBottom (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
   let ρ' = Empty :+: Bind x V.ValBottom :+: Bind y V.ValBottom
   in  Just $ T3 ρ' e Bottom
-- cons
fwd_match (V.ValCons u v) (ElimList { bnil: _, bcons: { x, y, e } }) (MatchCons mx mxs) =
   let ρ' = Empty :+: Bind x u :+: Bind y v
   in  Just $ T3 ρ' e Bottom
-- failure
fwd_match _ _ _ =  Nothing


-- TODO: remove Partial typeclass.
fwd :: Partial => Env -> Expr -> Expl -> Availability -> Val
-- var
fwd ρ (ExprVar x) t α =
   case find x ρ of
      Just val -> val
      _        -> V.ValFailure ("variable " <> x <> " not found")
-- true-sel
fwd ρ ExprTrueSel ExplTrue Top                        = V.ValTrueSel
-- true-bot
fwd ρ ExprTrueSel ExplTrue Bottom                     = V.ValBottom
-- true
fwd ρ ExprTrue ExplTrue _                             = V.ValBottom
-- false-sel
fwd ρ ExprFalseSel ExplFalse Top                      = V.ValFalseSel
-- false-bot
fwd ρ ExprFalseSel ExplFalse Bottom                   = V.ValBottom
-- false-bot
fwd ρ ExprFalse ExplFalse _                           = V.ValBottom
-- int-sel
fwd ρ (ExprIntSel n) (ExplInt tn) Top                 = V.ValIntSel n
-- int-bot
fwd ρ (ExprIntSel n) (ExplInt tn) Bottom              = V.ValBottom
-- int
fwd ρ (ExprInt n) (ExplInt tn) _                      = V.ValBottom
-- pair-sel
fwd ρ (ExprPairSel e1 e2) (ExplPair te1 te2) Top      = V.ValPairSel (fwd ρ e1 te1 Top) (fwd ρ e2 te2 Top)
-- pair
fwd ρ (ExprPair e1 e2) (ExplPair te1 te2) α           = V.ValPair (fwd ρ e1 te1 α) (fwd ρ e2 te2 α)
-- nil-sel
fwd ρ ExprNilSel ExplNil Top                          = V.ValNilSel
-- nil-bot
fwd ρ ExprNilSel ExplNil Bottom                       = V.ValBottom
-- nil
fwd ρ ExprNil ExplNil _                               = V.ValBottom
-- cons-sel
fwd ρ (ExprConsSel e es) (ExplCons te tes) Top        = V.ValConsSel (fwd ρ e te Top) (fwd ρ es tes Top)
-- cons-sel
fwd ρ (ExprCons e es) (ExplCons te tes) α             = V.ValCons (fwd ρ e te α) (fwd ρ es tes α)
-- letrec (fun)
fwd ρ (ExprLetrec f σ e) (ExplLetrec x tσ te) α = fwd (ρ :+: Bind f (V.ValClosure ρ f σ)) e te α
-- apply
fwd ρ (ExprApp e e') (ExplApp te te' m tu) α =
   case fwd ρ e te α  of
      V.ValClosure ρ' f σ ->
         case fwd_match (fwd ρ e' te' α) σ m of
            Just (T3 ρ'' e''  α') -> fwd (ρ' :++: ρ'' :+: Bind f (V.ValClosure ρ' f σ)) e'' tu α'
            Nothing               -> V.ValFailure "Match not found"
      _  -> V.ValFailure "Impossible"
-- add-bot
fwd ρ (ExprAdd e1 e2) (ExplAdd te1 te2) Bottom   = V.ValBottom
fwd ρ (ExprAdd e1 e2) (ExplAdd te1 te2) Top =
   let v1 = fwd ρ e1 te1 Top
       v2 = fwd ρ e2 te2 Top
   in case v1, v2 of
      -- add
      (V.ValInt n1), (V.ValInt n2) -> V.ValInt (n1 + n2)
      -- add-bot-1
      V.ValBottom,  _            -> V.ValBottom
      -- add-bot-2
      _,          V.ValBottom    -> V.ValBottom
      _,          _            -> V.ValFailure "Impossible"
-- let
fwd ρ (ExprLet x e1 e2) (ExplLet tx te1 te2) α =
   let v1  = fwd ρ e1 te1 α
       ρ'  = (ρ :+: Bind x v1)
   in  fwd ρ' e2 te2 α
-- match (no rule in paper)
fwd ρ (ExprMatch e σ) (ExplMatch te m tu) α =
   case fwd_match (fwd ρ e te α) σ m of
      Nothing            -> V.ValFailure "Impossible"
      Just (T3 ρ' e' α') -> fwd (ρ :++: ρ') e' tu α'
