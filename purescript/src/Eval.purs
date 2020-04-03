module Eval where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expr


match :: Val -> Elim -> Maybe (T3 Env Expr Match)
match val σ
 = case  val, σ of 
    _, ElimVar x t expr 
        ->  Just $ T3 (EnvNil :∈: T2 x val) expr (MatchVar x)
    ValTrue, ElimBool (BranchTrue expr1) (BranchFalse expr2)
        ->  Just $ T3 EnvNil expr1 MatchTrue
    ValFalse, ElimBool (BranchTrue expr1) (BranchFalse expr2)
        ->  Just $ T3 EnvNil expr2 MatchFalse
    ValPair x' y', ElimPair x _ y _ expr
        ->  let ρ' = (EnvNil :∈: T2 y y' :∈: T2 x x')
            in  Just $ T3 ρ' expr (MatchPair x y)
    ValNil, ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) 
        ->  Just $ T3 EnvNil expr2 MatchNil
    ValCons v vs, ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) 
        ->  let ρ' = (EnvNil :∈: T2 xs vs :∈: T2 x v)
            in  Just $ T3 ρ' expr1 (MatchCons x xs)
    _, _ ->  Nothing




eval :: Partial => Expr -> Env -> T2 Trace Val
eval ExprBottom ρ = T2 TraceBottom ValBottom
eval (ExprVar x) ρ
 = case findVarVal x ρ of
    Just val -> T2 (TraceVar x) val
    _        -> T2 (TraceBottom) (ValFailure ("variable " <> x <> " not found"))
eval ExprTrue  ρ             = T2 TraceTrue ValTrue
eval ExprFalse  ρ            = T2 TraceFalse ValFalse
eval (ExprNum n)  ρ          = T2 (TraceNum n) (ValNum n)
eval (ExprPair e1 e2)  ρ     
 = let T2 t1 v1 = eval e1 ρ
       T2 t2 v2 = eval e2 ρ
   in  T2 (TracePair t1 t2) (ValPair v1 v2) 
eval (ExprLetrec fun σ e)  ρ   = let T2 t v = eval e (ρ :∈: T2 fun (ValClosure ρ fun σ))
                                     t'     = TraceLetrec fun (TraceClosure ρ σ) t 
                                 in  T2 t' v
eval (ExprApp e e')  ρ
 = case eval e ρ  of
     T2 t (ValClosure ρ' fun σ)
        -> let T2 t' v = eval e' ρ
           in case match v σ of
                Just (T3 ρ'' e'' m) -> let T2 u v' = eval e'' (concEnv ρ' ρ'' :∈: T2 fun (ValClosure ρ' fun σ))
                                       in  T2 (TraceApp t t' m u) v'
                Nothing           -> T2 TraceBottom (ValFailure "Match not found")
     _  -> T2 TraceBottom (ValFailure "Applied expression e in e e' does not evaluate to closure")
eval (ExprAdd e1 e2) ρ
 = let T2 t1 v1 = eval e1 ρ
       T2 t2 v2 = eval e2 ρ
   in  case v1, v2 of
          (ValNum n1), (ValNum n2) -> T2 (TraceAdd t1 t2) (ValNum (n1 + n2))
          _,          _            -> T2 TraceBottom (ValFailure "Arithemetic type error: e1 or/and e2 do not evaluate to ints")
eval (ExprLet x e1 e2) ρ
 = let T2 t1 v1  = eval e1 ρ
       ρ'  = (ρ :∈: T2 x v1)
       T2 t2 v2  = eval e2 ρ'
   in  T2 (TraceLet x t1 t2) (v2)
eval ExprNil ρ               = T2 TraceNil ValNil
eval (ExprCons e es)  ρ      
 = let T2 t1 v1 = (eval e ρ) 
       T2 t2 v2 = (eval es ρ)
   in  T2 (TraceCons t1 t2) (ValCons v1 v2)
eval (ExprMatch e σ) ρ
 = let T2 t1 v1 =  (eval e ρ) 
   in case (match v1 σ) of
        Nothing            -> T2 TraceBottom (ValFailure "Match not found")
        Just (T3 ρ' e' m)  -> let T2 t2 v2 = eval e' (concEnv ρ ρ')
                              in  T2 (TraceMatch t1 m t2) v2



