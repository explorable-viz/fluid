module Test.PrettyTesting where 

import Pretty 
import Expr 

textExpr1 :: Expr 
textExpr1 = ExprLet "x" (ExprInt 5) (ExprAdd (ExprVar "x") (ExprInt 3))

textExpr2 :: Expr 
textExpr2 = ExprLet "x" (ExprInt 5) (ExprLet "y" (ExprInt 3) (ExprAdd (ExprVar "x") (ExprVar "y")))

