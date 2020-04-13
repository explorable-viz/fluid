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


fwd :: Partial => Expr -> Expl -> Availability -> Env -> Val
-- bot
fwd (ExprBottom) t α ρ = ValBottom
-- var
fwd (ExprVar x) t α ρ
 = case find x ρ of
    Just val -> val
    _        -> ValFailure ("variable " <> x <> " not found")
-- true
fwd ExprTrue ExplTrue Top  ρ                      = ValTrue
-- true-bot
fwd ExprTrue ExplTrue Bottom  ρ                   = ValBottom
-- false
fwd ExprFalse ExplFalse Top ρ                     = ValFalse
-- false-bot
fwd ExprFalse ExplFalse Bottom ρ                  = ValBottom
-- int
fwd (ExprInt n) (ExplInt tn) Top ρ                = ValInt n
-- int-bot
fwd (ExprInt n) (ExplInt tn) Bottom ρ             = ValBottom
-- pair
fwd (ExprPair e1 e2) (ExplPair te1 te2) Top  ρ    = ValPair (fwd e1 te1 Top ρ) (fwd e2 te2 Top ρ)
-- pair-botl or pair-botr
fwd (ExprPair e1 e2) (ExplPair te1 te2) Bottom ρ  = case (fwd e1 te1 Bottom ρ) of
                                                        -- pair-botr
                                                        ValBottom -> ValPairSnd (fwd e2 te2 Bottom ρ)
                                                        -- pair-botl
                                                        v         -> ValPairFst v
-- pair-projl
fwd (ExprPairFst e1) (ExplPair te1 te2) α ρ   = ValPairFst (fwd e1 te1 α ρ)
-- pair-projr
fwd (ExprPairSnd e2) (ExplPair te1 te2) α ρ   = ValPairSnd (fwd e2 te2 α ρ)
-- nil
fwd ExprNil ExplNil Top ρ                     = ValNil
-- nil-bot
fwd ExprNil ExplNil Bottom ρ                  = ValBottom
-- cons
fwd (ExprCons e es) (ExplCons te tes) Top ρ       = ValCons (fwd e te Top ρ) (fwd es tes Top ρ)
-- cons-bot-head or cons-bot-tail
fwd (ExprCons e es) (ExplCons te tes) Bottom ρ = case (fwd e te Bottom ρ) of
                                                    -- cons-bot-tail
                                                    ValBottom -> ValConsTail (fwd es tes Bottom ρ)
                                                    -- cons-bot-head
                                                    vs        -> ValConsHead vs
-- cons-proj-tail
fwd (ExprConsTail es) (ExplCons te tes) α ρ = ValConsTail (fwd es tes α ρ)
-- cons-proj-head
fwd (ExprConsHead e) (ExplCons te tes) α ρ = ValConsHead (fwd e te α ρ)
-- letrec (fun)
fwd (ExprLetrec fun σ e) (ExplLetrec x tσ te) α ρ = fwd e te α (ρ :+: Bind fun (ValClosure ρ fun σ))
-- apply
fwd (ExprApp e e') (ExplApp te te' m tu) α ρ
 = case fwd e te α ρ  of
     ValClosure ρ' fun σ
        -> case fwd_match (fwd e' te' α ρ) σ m of
                Just (T3 ρ'' e''  α') -> fwd e'' tu α' (ρ' :++: ρ'' :+: Bind fun (ValClosure ρ' fun σ))
                Nothing               -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not fwd to closure"
-- add-bot
fwd (ExprAdd e1 e2) (ExplAdd te1 te2) Bottom ρ   = ValBottom
fwd (ExprAdd e1 e2) (ExplAdd te1 te2) Top ρ
 = let v1 = fwd e1 te1 Top  ρ
       v2 = fwd e2 te2 Top  ρ
   in  case v1, v2 of
          -- add
          (ValInt n1), (ValInt n2) -> ValInt (n1 + n2)
          -- add-bot-1
          ValBottom,  _            -> ValBottom
          -- add-bot-2
          _,          ValBottom    -> ValBottom
          _,          _            -> ValFailure "Arithmetic type error: e1 or/and e2 do not fwd to ints"
-- let
fwd (ExprLet x e1 e2) (ExplLet tx te1 te2) α ρ
 = let v1  = fwd e1 te1 α ρ
       ρ'  = (ρ :+: Bind x v1)
   in  fwd e2 te2 α ρ'
-- let-proj
fwd (ExprLetBody x e1 e2) (ExplLet tx te1 te2) α ρ = fwd e2 te2 α (ρ :+: Bind x ValBottom)
-- match (no rule in paper)
fwd (ExprMatch e σ) (ExplMatch te m tu) α ρ
 = case fwd_match (fwd e te α ρ) σ m of
    Nothing            -> ValFailure "Match not found"
    Just (T3 ρ' e' α') -> fwd e' tu α' (ρ :++: ρ')

