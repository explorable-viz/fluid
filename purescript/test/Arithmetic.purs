module Test.Arithmetic where

-- import Expr (Env(..), Expr(..), Val(..), Ctx(..), Typ(..))
-- import Eval (eval)
-- import Prelude ((==))
-- import Typ (typeOf)
-- import Data.HeytingAlgebra ((&&))
-- --  isEmpty :: Expr -> Boolean
-- -- isEmpty (ExprCons x xs)

-- expr1 :: Expr
-- expr1 = (ExprAdd (ExprNum 5) (ExprAdd (ExprNum 3) (ExprNum 2)))

-- testAdd1 :: Boolean
-- testAdd1 = eval expr1 EnvNil == ValNum 10



-- testAdd2 :: Boolean
-- testAdd2 = let b1 = ((typeOf expr1 CtxNil) == (typeOf (eval expr1 EnvNil) CtxNil)) 
--                b2 = ((typeOf expr1 CtxNil) == TypNum)
--            in b1 && b2