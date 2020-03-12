module List where 
 
import Eval (eval)
import Expr (Env(..), Expr(..), Val(..))

import Prelude ((==))
import Data.Tuple (Tuple(..))
--  isEmpty :: Expr -> Boolean
-- isEmpty (ExprCons x xs)

expr1 :: Expr 
expr1 = (ExprCons (ExprNum 5) (ExprCons (ExprVar "x") (ExprCons (ExprTrue) ExprNil)))

testList1 :: Boolean
testList1 
 = eval expr1 (EnvSnoc EnvNil (Tuple "x" (ValNum 7))) == (ValCons (ValNum 5) (ValCons (ValNum 7) (ValCons (ValTrue) ValNil))) 