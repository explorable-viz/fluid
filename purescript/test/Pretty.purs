module Test.Pretty where

import Expr (Expr(..))
import Primitive (add)


prettyExpr1 :: Expr
prettyExpr1 = Let "x" (Int 5) (BinaryApp add (Var "x") (Int 3))

prettyExpr2 :: Expr
prettyExpr2 = Let "x" (Int 5) (Let "y" (Int 3) (BinaryApp add (Var "x") (Var "y")))

prettyExpr3 :: Expr
prettyExpr3 = Cons (Int 5) (Cons (Int 4) (Cons (Int 3) Nil))
