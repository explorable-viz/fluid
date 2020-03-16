module Test.Arithmetic where

import Expr (Env(..), Expr(..), Val(..))
import Eval (eval)
import Prelude ((==))
--  isEmpty :: Expr -> Boolean
-- isEmpty (ExprCons x xs)

expr1 :: Expr
expr1 = (ExprAdd (ExprNum 5) (ExprAdd (ExprNum 3) (ExprNum 2)))

testAdd1 :: Boolean
testAdd1 = eval expr1 EnvNil == ValNum 10
