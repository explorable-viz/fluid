module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expr
import Expl (Expl(..), Match(..))

fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Availability)
fwd_match val σ ξ
 = case val, σ, ξ of
    -- var
    _, ElimVar { x, tx, e }, MatchVar mx
        ->  Just $ T3 (Empty :+: Bind x val) e Top
    -- true
    ValTrue, ElimBool { btrue: e, bfalse: _ }, MatchTrue
        ->  Just $ T3 Empty e Top
    -- true-del
    ValBottom, ElimBool { btrue: e, bfalse: _ }, MatchTrue
        ->  Just $ T3 Empty e Bottom
    -- false
    ValFalse, ElimBool { btrue: _, bfalse: e }, MatchFalse
        ->  Just $ T3 Empty e Top
    -- false-del
    ValBottom, ElimBool { btrue: _, bfalse: e }, MatchFalse
        ->  Just $ T3 Empty e Bottom
    -- pair
    ValPair x' y', ElimPair { x, y, e }, MatchPair mx my
        ->  let ρ' = Empty :+: Bind x x' :+: Bind y y'
            in  Just $ T3 ρ' e Top
    -- pair-projl
    ValPairFst x', ElimPair { x, y, e }, MatchPair mx my
        ->  let ρ' = Empty :+: Bind x x' :+: Bind y ValBottom
            in  Just $ T3 ρ' e Bottom
    -- pair-projr
    ValPairSnd y', ElimPair { x, y, e }, MatchPair mx my
        ->  let ρ' = Empty :+: Bind x ValBottom :+: Bind y y'
            in  Just $ T3 ρ' e Bottom
    -- nil
    ValNil, ElimList { bnil: e, bcons: _ }, MatchNil
        ->  Just $ T3 Empty e Top
    -- nil-del
    ValBottom, ElimList { bnil: e, bcons: _ }, MatchNil
        ->  Just $ T3 Empty e Bottom
    -- cons
    ValCons v v', ElimList { bnil: _, bcons: { x, y, e } }, MatchCons mx mxs
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v'
            in  Just $ T3 ρ' e Top
    -- cons-projhead
    ValConsHead v, ElimList { bnil: _, bcons: { x, y, e } }, MatchCons mx mxs
        ->  let ρ' = Empty :+: Bind x v :+: Bind y ValBottom
            in  Just $ T3 ρ' e Bottom
    -- cons-projtail
    ValConsTail v', ElimList { bnil: _, bcons: { x, y, e } }, MatchCons mx mxs
        ->  let ρ' = Empty :+: Bind x ValBottom :+: Bind y v'
            in  Just $ T3 ρ' e Bottom
    -- failure
    _,_,_ ->  Nothing


-- TODO: remove Partial typeclass.
fwd :: Partial => Env -> Expr -> Expl -> Availability -> Val
-- bot
fwd _ ExprBottom _ _ = ValBottom
-- var
fwd ρ (ExprVar x) t α
 = case find x ρ of
    Just val -> val
    _        -> ValFailure ("variable " <> x <> " not found")
-- true
fwd ρ ExprTrue ExplTrue Top                        = ValTrue
-- true-bot
fwd ρ ExprTrue ExplTrue Bottom                     = ValBottom
-- false
fwd ρ ExprFalse ExplFalse Top                     = ValFalse
-- false-bot
fwd ρ ExprFalse ExplFalse Bottom                  = ValBottom
-- int
fwd ρ (ExprInt n) (ExplInt tn) Top                = ValInt n
-- int-bot
fwd ρ (ExprInt n) (ExplInt tn) Bottom             = ValBottom
-- pair
fwd ρ (ExprPair e1 e2) (ExplPair te1 te2) Top      = ValPair (fwd ρ e1 te1 Top) (fwd ρ e2 te2 Top)
-- pair-botl or pair-botr
fwd ρ (ExprPair e1 e2) (ExplPair te1 te2) Bottom  = case (fwd ρ e1 te1 Bottom) of
                                                        -- pair-botr
                                                        ValBottom -> ValPairSnd (fwd ρ e2 te2 Bottom)
                                                        -- pair-botl
                                                        v         -> ValPairFst v
-- pair-projl
fwd ρ (ExprPairFst e1) (ExplPair te1 te2) α   = ValPairFst (fwd ρ e1 te1 α)
-- pair-projr
fwd ρ (ExprPairSnd e2) (ExplPair te1 te2) α   = ValPairSnd (fwd ρ e2 te2 α)
-- nil
fwd ρ ExprNil ExplNil Top                     = ValNil
-- nil-bot
fwd ρ ExprNil ExplNil Bottom                  = ValBottom
-- cons
fwd ρ (ExprCons e es) (ExplCons te tes) Top       = ValCons (fwd ρ e te Top) (fwd ρ es tes Top)
-- cons-bot-head or cons-bot-tail
fwd ρ (ExprCons e es) (ExplCons te tes) Bottom = case (fwd ρ e te Bottom) of
                                                    -- cons-bot-tail
                                                    ValBottom -> ValConsTail (fwd ρ es tes Bottom)
                                                    -- cons-bot-head
                                                    vs        -> ValConsHead vs
-- cons-proj-tail
fwd ρ (ExprConsTail es) (ExplCons te tes) α = ValConsTail (fwd ρ es tes α)
-- cons-proj-head
fwd ρ (ExprConsHead e) (ExplCons te tes) α = ValConsHead (fwd ρ e te α)
-- letrec (fun)
fwd ρ (ExprLetrec f σ e) (ExplLetrec x tσ te) α = fwd (ρ :+: Bind f (ValClosure ρ f σ)) e te α
-- apply
fwd ρ (ExprApp e e') (ExplApp te te' m tu) α
 = case fwd ρ e te α  of
     ValClosure ρ' f σ
        -> case fwd_match (fwd ρ e' te' α) σ m of
                Just (T3 ρ'' e''  α') -> fwd (ρ' :++: ρ'' :+: Bind f (ValClosure ρ' f σ)) e'' tu α'
                Nothing               -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not fwd to closure"
-- add-bot
fwd ρ (ExprAdd e1 e2) (ExplAdd te1 te2) Bottom   = ValBottom
fwd ρ (ExprAdd e1 e2) (ExplAdd te1 te2) Top
 = let v1 = fwd ρ e1 te1 Top
       v2 = fwd ρ e2 te2 Top
   in  case v1, v2 of
          -- add
          (ValInt n1), (ValInt n2) -> ValInt (n1 + n2)
          -- add-bot-1
          ValBottom,  _            -> ValBottom
          -- add-bot-2
          _,          ValBottom    -> ValBottom
          _,          _            -> ValFailure "Arithmetic type error: e1 or/and e2 do not fwd to ints"
-- let
fwd ρ (ExprLet x e1 e2) (ExplLet tx te1 te2) α
 = let v1  = fwd ρ e1 te1 α
       ρ'  = (ρ :+: Bind x v1)
   in  fwd ρ' e2 te2 α
-- let-proj
fwd ρ (ExprLetBody x e1 e2) (ExplLet tx te1 te2) α = fwd (ρ :+: Bind x ValBottom) e2 te2 α
-- match (no rule in paper)
fwd ρ (ExprMatch e σ) (ExplMatch te m tu) α
 = case fwd_match (fwd ρ e te α) σ m of
    Nothing            -> ValFailure "Match not found"
    Just (T3 ρ' e' α') -> fwd (ρ :++: ρ') e' tu α'

