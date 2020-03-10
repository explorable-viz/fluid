module Eval where 

import Prelude ((<<<), (==), (<>), ($))
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), (!!), find)
import Data.Eq 
import Data.Maybe 
import Effect.Exception

import Lang 

eval :: Expr -> Env -> Value 
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
eval (ExprMatch e Nil) env
 = ValFailure ("Match not found")
eval (ExprMatch e (Cons elim elims)) env
 = let v = eval e env
       elimv = match v elim 
   in  case elimv of ValFailure _ -> eval (ExprMatch e elims) env
                     _            -> elimv
   where match v elim = case Tuple v elim of 
                            Tuple _ (ElimVar x expr) 
                                ->  let env' = ((Tuple x v):env)
                                    in  eval expr env' 
                            Tuple ValNil (ElimNil expr)  
                                -> eval expr env 
                            Tuple (ValCons y ys) (ElimCons x xs expr) 
                                ->  let env' = ((Tuple x y):(Tuple xs ys):env)
                                    in  eval expr env'
                            Tuple (ValPair x' y') (ElimPair x y expr)
                                ->  let env' = ((Tuple x x'):(Tuple y y'):env)
                                    in  eval expr env'
                            _ ->   ValFailure ("Match not found")
eval (ExprFunc elim) env 
 = ValClosure env elim

