module Test.Pair where

-- import Expr (Env(..), Expr(..), Val(..), Ctx(..), Typ(..))
-- import Eval (eval)
-- import Prelude ((==))
-- import Typ (typeOf)
-- import Data.Tuple (Tuple(..))
-- import Data.HeytingAlgebra ((&&))
-- --  isEmpty :: Expr -> Boolean
-- -- isEmpty (ExprCons x xs)

-- expr1 :: Expr
-- expr1 = ExprPair (ExprNum 5) (ExprPair (ExprVar "x") (ExprNum 3))

-- testPair1 :: Boolean
-- testPair1 = eval expr1 (EnvSnoc EnvNil (Tuple "x" (ValNum 10))) == ValPair (ValNum 5) (ValPair (ValNum 10) (ValNum 3))



-- testPair2 :: Boolean
-- testPair2 = let env = EnvSnoc EnvNil (Tuple "x" (ValNum 10))
--                 ctx = CtxSnoc CtxNil (Tuple "x" (TypNum))
--                 b1 = typeOf expr1 ctx == typeOf (eval expr1 env) ctx
--                 b2 = typeOf expr1 ctx == TypPair TypNum (TypPair TypNum TypNum)
--             in b1 && b2