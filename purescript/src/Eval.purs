module Eval where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expr


match :: Val -> Elim -> Maybe (T3 Env Expr Availability)
match val σ
 = case T2 val σ of 
    T2 _ (ElimVar x t expr) 
        ->  Just $ T3 (EnvNil :∈: T2 x val) expr Top
    T2 (ValTrue) (ElimBool (BranchTrue expr1) (BranchFalse expr2))
        ->  Just $ T3 EnvNil expr1 Top
    T2 (ValFalse) (ElimBool (BranchTrue expr1) (BranchFalse expr2))
        ->  Just $ T3 EnvNil expr2 Top
    T2 (ValPair x' y') (ElimPair x _ y _ expr)
        ->  let ρ' = (EnvNil :∈: T2 y y' :∈: T2 x x')
            in  Just $ T3 ρ' expr Top
    T2 (ValPair_Del x' y') (ElimPair x _ y _ expr)
        ->  let ρ' = (EnvNil :∈: T2 y y' :∈: T2 x x')
            in  Just $ T3 ρ' expr Bottom
    T2 (ValNil) (ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) )
        ->  Just $ T3 EnvNil expr2 Top
    T2 (ValCons v vs) (ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) )
        ->  let ρ' = (EnvNil :∈: T2 xs vs :∈: T2 x v)
            in  Just $ T3 ρ' expr1 Top
    T2 (ValCons_Del v vs) (ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) )
        ->  let ρ' = (EnvNil :∈: T2 xs vs :∈: T2 x v)
            in  Just $ T3 ρ' expr1 Bottom
    _   ->  Nothing




eval :: Expr -> Availability -> Env -> Val
eval ExprBottom α ρ             = ValBottom
eval (ExprVar x) α ρ
 = case findVarVal x ρ of
    Just val -> val
    _        -> ValFailure ("variable " <> x <> " not found")
eval ExprTrue Top ρ             = ValTrue
eval ExprTrue Bottom ρ          = ValBottom
eval ExprFalse Top ρ            = ValFalse
eval ExprFalse Bottom ρ         = ValBottom
eval (ExprNum n) Top ρ          = ValNum n
eval (ExprNum n) Bottom ρ       = ValBottom
eval (ExprPair e1 e2) Top ρ     = ValPair (eval e1 Top ρ) (eval e2 Top ρ)
eval (ExprPair e1 e2) Bottom ρ  = ValPair_Del (eval e1 Bottom ρ) (eval e2 Bottom ρ)
eval (ExprPair_Del e1 e2) α ρ   = ValPair_Del (eval e1 α ρ) (eval e2 α ρ)
eval (ExprFun fun σ) α ρ        = ValClosure ρ fun σ
eval (ExprApp e e') α ρ
 = case eval e α ρ  of
     ValClosure ρ' fun σ
        -> case match (eval e' α ρ) σ of
                   Just (T3 ρ'' e''  α') -> eval e'' α' (concEnv ρ' ρ'' :∈: T2 fun (ValClosure ρ' fun σ))
                   Nothing               -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not evaluate to closure"
eval (ExprAdd e1 e2) Bottom ρ   = ValBottom
eval (ExprAdd e1 e2) Top ρ
 = let v1 = eval e1 Top ρ
       v2 = eval e2 Top ρ
   in  case T2 v1 v2 of
         T2 (ValNum n1) (ValNum n2)  -> ValNum (n1 + n2)
         T2 ValBottom   _            -> ValBottom 
         T2 _           ValBottom    -> ValBottom
         _                           -> ValFailure "Arithemetic type error: e1 or/and e2 do not evaluate to ints"
eval (ExprLet x e1 e2) α ρ
 = let v1  = eval e1 α ρ
       ρ'  = (ρ :∈: T2 x v1)
   in  eval e2 α ρ'
eval (ExprLet_Body x e1 e2) α ρ = eval e2 α (ρ :∈: T2 x ValBottom)
eval ExprNil α ρ                = ValNil
eval (ExprCons e es) Top ρ      = ValCons (eval e Top ρ) (eval es Top ρ)
eval (ExprCons e es) Bottom ρ   = ValCons_Del (eval e Bottom ρ) (eval es Bottom ρ)
eval (ExprCons_Del e es) α ρ    = ValCons_Del (eval e α ρ) (eval es α ρ)
eval (ExprMatch e σ) α ρ
 = case match (eval e α ρ) σ of
    Nothing            -> ValFailure "Match not found"
    Just (T3 ρ' e' α') -> eval e' α' (concEnv ρ ρ')



-- project :: 