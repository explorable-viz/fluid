module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expr
import Expl

fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Availability)
fwd_match val σ ξ
 = case val, σ, ξ of 
    -- var
    _, ElimVar x t expr, MatchVar mx
        ->  Just $ T3 (Empty :+: Bind x val) expr Top
    -- true
    ValTrue, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchTrue
        ->  Just $ T3 Empty expr1 Top
    -- true-del
    ValBottom, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchTrue
        ->  Just $ T3 Empty expr1 Bottom
    -- false
    ValFalse, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchFalse
        ->  Just $ T3 Empty expr2 Top
    -- false-del
    ValBottom, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchFalse
        ->  Just $ T3 Empty expr2 Bottom
    -- pair
    ValPair x' y', ElimPair x _ y _ expr, MatchPair mx my 
        ->  let ρ' = (Empty :+: Bind y y' :+: Bind x x')
            in  Just $ T3 ρ' expr Top
    -- pair-del (no current rule in paper)
    ValPair_Del x' y', ElimPair x _ y _ expr, MatchPair mx my
        ->  let ρ' = (Empty :+: Bind y y' :+: Bind x x')
            in  Just $ T3 ρ' expr Bottom
    -- nil
    ValNil, ElimList (BranchNil expr2) (BranchCons x xs _ expr1), MatchNil
        ->  Just $ T3 Empty expr2 Top
    -- nil-del
    ValBottom, ElimList (BranchNil expr2) (BranchCons x xs _ expr1), MatchNil
        ->  Just $ T3 Empty expr2 Bottom   
    -- cons    
    ValCons v vs, ElimList (BranchNil expr2) (BranchCons x xs _ expr1), MatchCons mx mxs
        ->  let ρ' = (Empty :+: Bind xs vs :+: Bind x v)
            in  Just $ T3 ρ' expr1 Top
    -- cons-del
    ValCons_Del v vs, ElimList (BranchNil expr2) (BranchCons x xs _ expr1), MatchCons mx mxs
        ->  let ρ' = (Empty :+: Bind xs vs :+: Bind x v)
            in  Just $ T3 ρ' expr1 Bottom
    -- failure
    _,_,_ ->  Nothing

fwd :: Partial => Expr -> Expl -> Availability -> Env -> Val
-- bot
fwd (ExprBottom) ExplBottom α ρ = ValBottom
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
fwd (ExprPair e1 e2) (ExplPair te1 te2) Bottom ρ  = ValPair_Del (fwd e1 te1 Bottom ρ) (fwd e2 te2 Bottom ρ)
-- pair-projl or pair-projr
fwd (ExprPair_Del e1 e2) (ExplPair te1 te2) α ρ   = ValPair_Del (fwd e1 te1 α ρ) (fwd e2 te2 α ρ)
-- nil
fwd ExprNil ExplNil α ρ                           = ValNil
-- cons
fwd (ExprCons e es) (ExplCons te tes) Top ρ       = ValCons (fwd e te Top ρ) (fwd es tes Top ρ)
-- cons-bot-head or cons-bot-tail
fwd (ExprCons e es) (ExplCons te tes) Bottom ρ    = ValCons_Del (fwd e te Bottom ρ) (fwd es tes Bottom ρ)
-- cons-proj-head or cons-proj-tail
fwd (ExprCons_Del e es) (ExplCons te tes) α ρ     = ValCons_Del (fwd e te α ρ) (fwd es tes α ρ)
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
fwd (ExprLet_Body x e1 e2) (ExplLet tx te1 te2) α ρ = fwd e2 te2 α (ρ :+: Bind x ValBottom)
-- match (no rule in paper)
fwd (ExprMatch e σ) (ExplMatch te m tu) α ρ
 = case fwd_match (fwd e te α ρ) σ m of
    Nothing            -> ValFailure "Match not found"
    Just (T3 ρ' e' α') -> fwd e' tu α' (ρ :++: ρ')

