module Fwd where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expl (Match(..), Expl(..))
import Expr


fwd_match :: Val -> Elim -> Match -> Maybe (T3 Env Expr Availability)
fwd_match val σ ξ
 = case val, σ, ξ of
    _, ElimVar x t expr, MatchVar mx
        ->  Just $ T3 (Empty :∈: Bind x val) expr Top
    ValTrue, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchTrue
        ->  Just $ T3 Empty expr1 Top
    ValBottom, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchTrue
        ->  Just $ T3 Empty expr1 Bottom
    ValFalse, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchFalse
        ->  Just $ T3 Empty expr2 Top
    ValBottom, ElimBool (BranchTrue expr1) (BranchFalse expr2), MatchFalse
        ->  Just $ T3 Empty expr2 Bottom
    ValPair x' y', ElimPair x _ y _ e, MatchPair mx my
        ->  let ρ' = (Empty :∈: Bind y y' :∈: Bind x x')
            in  Just $ T3 ρ' e Top
    ValPair_Del x' y', ElimPair x _ y _ e, MatchPair mx my
        ->  let ρ' = (Empty :∈: Bind y y' :∈: Bind x x')
            in  Just $ T3 ρ' e Bottom
    ValNil, ElimList (BranchNil _ e2) (BranchCons _ _ _ _), MatchNil
        ->  Just $ T3 Empty e2 Top
    ValBottom, ElimList (BranchNil _ e2) (BranchCons _ _ _ _), MatchNil
        ->  Just $ T3 Empty e2 Bottom
    ValCons v v', ElimList (BranchNil _ _) (BranchCons x y _ e1), MatchCons mx mxs
        ->  let ρ' = Empty :∈: Bind y v' :∈: Bind x v -- todo: are these bindings in the wrong order?
            in  Just $ T3 ρ' e1 Top
    ValCons_Del v v', ElimList (BranchNil _ _) (BranchCons x y _ e1), MatchCons mx mxs
        ->  let ρ' = Empty :∈: Bind y v' :∈: Bind x v -- ditto
            in  Just $ T3 ρ' e1 Bottom
    _,_,_ ->  Nothing




fwd :: Partial => Expr -> Expl -> Availability -> Env -> Val
fwd (ExprBottom) ExplBottom α ρ = ValBottom
fwd (ExprVar x) t α ρ
 = case find x ρ of
    Just val -> val
    _        -> ValFailure ("variable " <> x <> " not found")
fwd ExprTrue ExplTrue Top  ρ                    = ValTrue
fwd ExprTrue ExplTrue Bottom  ρ                 = ValBottom
fwd ExprFalse ExplFalse Top ρ                   = ValFalse
fwd ExprFalse ExplFalse Bottom ρ                = ValBottom
fwd (ExprNum n) (ExplNum tn) Top ρ              = ValNum n
fwd (ExprNum n) (ExplNum tn) Bottom ρ           = ValBottom
fwd (ExprPair e1 e2) (ExplPair te1 te2) Top  ρ  = ValPair (fwd e1 te1 Top ρ) (fwd e2 te2 Top ρ)
fwd (ExprPair e1 e2) (ExplPair te1 te2) Bottom ρ       = ValPair_Del (fwd e1 te1 Bottom ρ) (fwd e2 te2 Bottom ρ)
fwd (ExprPair_Del e1 e2) (ExplPair te1 te2) α ρ = ValPair_Del (fwd e1 te1 α ρ) (fwd e2 te2 α ρ)
fwd ExprNil ExplNil α ρ                = ValNil
fwd (ExprCons e es) (ExplCons te tes) Top ρ      = ValCons (fwd e te Top ρ) (fwd es tes Top ρ)
fwd (ExprCons e es) (ExplCons te tes) Bottom ρ   = ValCons_Del (fwd e te Bottom ρ) (fwd es tes Bottom ρ)
fwd (ExprCons_Del e es) (ExplCons te tes) α ρ    = ValCons_Del (fwd e te α ρ) (fwd es tes α ρ)
fwd (ExprLetrec fun σ e) (ExplLetrec x tσ te) α ρ = fwd e te α (ρ :∈: Bind fun (ValClosure ρ fun σ))
fwd (ExprApp e e') (ExplApp te te' m tu) α ρ
 = case fwd e te α ρ  of
     ValClosure ρ' fun σ
        -> case fwd_match (fwd e' te' α ρ) σ m of
                Just (T3 ρ'' e''  α') -> fwd e'' tu α' (conc ρ' ρ'' :∈: Bind fun (ValClosure ρ' fun σ))
                Nothing               -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not fwd to closure"
fwd (ExprAdd e1 e2) (ExplAdd te1 te2) Bottom ρ   = ValBottom
fwd (ExprAdd e1 e2) (ExplAdd te1 te2) Top ρ
 = let v1 = fwd e1 te1 Top  ρ
       v2 = fwd e2 te2 Top  ρ
   in  case v1, v2 of
          (ValNum n1), (ValNum n2) -> ValNum (n1 + n2)
          ValBottom,  _            -> ValBottom
          _,          ValBottom    -> ValBottom
          _,          _            -> ValFailure "Arithemetic type error: e1 or/and e2 do not fwd to ints"
fwd (ExprLet x e1 e2) (ExplLet tx te1 te2) α ρ
 = let v1  = fwd e1 te1 α ρ
       ρ'  = (ρ :∈: Bind x v1)
   in  fwd e2 te2 α ρ'
fwd (ExprLet_Body x e1 e2) (ExplLet tx te1 te2) α ρ = fwd e2 te2 α (ρ :∈: Bind x ValBottom)
fwd (ExprMatch e σ) (ExplMatch te m tu) α ρ
 = case fwd_match (fwd e te α ρ) σ m of
    Nothing            -> ValFailure "Match not found"
    Just (T3 ρ' e' α') -> fwd e' tu α' (conc ρ ρ')
