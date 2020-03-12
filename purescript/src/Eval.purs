module Eval where 

import Prelude ((<>), ($))
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), find)
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expr 

-- Given val, match against elim to maybe return a 
-- branch expr and updated env
match :: Val -> Elim -> Maybe (Tuple Expr Env)
match val elim
 = case Tuple val elim of 
    Tuple _ (ElimVar x expr) 
        ->  Just $ Tuple expr (EnvSnoc EnvNil (Tuple x val))
    Tuple (ValCons v vs) (ElimList (BranchNil expr2) (BranchCons x xs expr1) )
        ->  let env' = (EnvSnoc (EnvSnoc EnvNil (Tuple xs vs)) (Tuple x v))
            in  Just $ Tuple expr1 env' 
    Tuple (ValNil) (ElimList (BranchNil expr2) (BranchCons x xs expr1) )
        ->  Just $ Tuple expr2 EnvNil
    Tuple (ValPair x' y') (ElimPair x y expr)
        ->  let env' = (EnvSnoc (EnvSnoc EnvNil (Tuple y y')) (Tuple x x'))
            in  Just $ Tuple expr env'
    Tuple (ValTrue) (ElimBool (BranchTrue expr1) (BranchFalse expr2))
        ->  Just $ Tuple expr1 EnvNil
    Tuple (ValFalse) (ElimBool (BranchTrue expr1) (BranchFalse expr2))
        ->  Just $ Tuple expr2 EnvNil
    _   ->  Nothing


eval :: Expr -> Env -> Val
eval (ExprVar x) env 
 = case findVarVal x env of 
    Just val -> val
    _        -> ValFailure ("variable " <> x <> " not found")
eval (ExprPair e1 e2) env 
 = ValPair (eval e1 env) (eval e2 env)
eval (ExprLet x e1 e2) env 
 = let v1    = (eval e1 env)
       env'  = (EnvSnoc env (Tuple x v1))
   in  eval e2 env'
eval (ExprNum n) env 
 = ValNum n 
eval ExprTrue env 
 = ValTrue
eval ExprFalse env 
 = ValFalse
eval ExprNil env 
 = ValNil
eval (ExprCons e es) env 
 = ValCons (eval e env) (eval es env)
eval (ExprMatch e elim) env
 = case match (eval e env) elim of 
    Nothing              -> ValFailure "Match not found"
    Just (Tuple e' env') -> eval e' (concEnv env env')
eval (ExprFunc elim) env 
 = ValClosure env elim
eval (ExprApp e e') env 
 = case eval e env  of 
     ValClosure env' elim 
        -> case match (eval e' env) elim of 
                   Just (Tuple e'' env'') -> eval e'' (concEnv env' env'')
                   Nothing                -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not evaluate to closure"
eval (ExprAdd e1 e2) env 
 = let v1 = eval e1 env 
       v2 = eval e2 env 
   in  case Tuple v1 v2 of 
         Tuple (ValNum n1) (ValNum n2)  -> ValNum (n1 + n2)
         _                              -> ValFailure "Arithemetic type error: e1 or/and e2 do not evaluate to ints"
