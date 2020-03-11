module Eval where 

import Prelude ((==), (<>), ($))
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), find)
import Data.Maybe (Maybe(..))

import Lang 

-- Given val, match against elim to maybe return a 
-- branch expr and updated env
match :: Val -> Elim -> Maybe (Tuple Expr Env)
match val elim
 = case Tuple val elim of 
    Tuple _ (ElimVar x expr) 
        ->  Just $ Tuple expr ((Tuple x val):Nil)
    Tuple ValNil (ElimList Nothing expr)  
        ->  Just $ Tuple expr Nil
    Tuple (ValCons v vs) (ElimList (Just (Tuple x xs)) expr) 
        ->  let env' = ((Tuple x v):(Tuple xs vs):Nil)
            in  Just $ Tuple expr env' 
    Tuple (ValPair x' y') (ElimPair x y expr)
        ->  let env' = ((Tuple x x'):(Tuple y y'):Nil)
            in  Just $ Tuple expr env'
    _   ->  Nothing


eval :: Expr -> Env -> Val
eval (ExprVar x) env 
 = case find (\(Tuple var val) -> var == x) env of 
    Just (Tuple var val) -> val
    _                    -> ValFailure ("variable " <> x <> " not found")
eval (ExprPair e1 e2) env 
 = ValPair (eval e1 env) (eval e2 env)
eval (ExprLet x e1 e2) env 
 = let v1    = (eval e1 env)
       env'  = ((Tuple x v1): env)
   in  eval e2 env'
eval (ExprNum n) env 
 = ValNum n 
eval (ExprNil) env 
 = ValNil
eval (ExprCons e es) env 
 = ValCons (eval e env) (eval es env)
eval (ExprMatch e elim) env
 = case match (eval e env) elim of 
    Nothing              -> ValFailure "Match not found"
    Just (Tuple e' env') -> eval e' env'
eval (ExprFunc elim) env 
 = ValClosure env elim
eval (ExprApp e e') env 
 = case eval e env  of 
     ValClosure env' elim 
        -> case match (eval e' env) elim of 
                   Just (Tuple e'' env'') -> eval e'' (env' <> env'')
                   Nothing                -> ValFailure "Match not found"
     _  -> ValFailure "Applied expression e in e e' does not evaluate to closure"
       
