module Test.List where

-- import Eval (eval)
-- import Typ (typeOf)
-- import Expr (Env(..), Expr(..), Val(..), Ctx(..), Typ(..))
-- import Data.HeytingAlgebra ((&&))
-- import Prelude ((==))
-- import Data.Tuple (Tuple(..))
--  isEmpty :: Expr -> Boolean
-- isEmpty (ExprCons x xs)

-- expr1 :: Expr
-- expr1 = (ExprCons (ExprNum 5) (ExprCons (ExprVar "x") (ExprCons (ExprTrue) ExprNil)))

-- testList1 :: Boolean
-- testList1
--  = eval expr1 (EnvSnoc EnvNil (Tuple "x" (ValNum 7))) == (ValCons (ValNum 5) (ValCons (ValNum 7) (ValCons (ValTrue) ValNil)))

-- testList2 :: Boolean
-- testList2 = let b1 = typeOf expr1 CtxNil == typeOf (eval expr1 EnvNil) CtxNil
--                 b2 = typeOf expr1 CtxNil == TypList TypNum
--             in  b1 && b2