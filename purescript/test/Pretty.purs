module Test.Pretty where

import Expr (Expr(..))

prettyExpr1 :: Expr
prettyExpr1 = Let "x" (Int 5) (Add (Var "x") (Int 3))

prettyExpr2 :: Expr
prettyExpr2 = Let "x" (Int 5) (Let "y" (Int 3) (Add (Var "x") (Var "y")))

prettyExpr3 :: Expr
prettyExpr3 = Cons (Int 5) (Cons (Int 4) (Cons (Int 3) Nil))

-- prettyExpr4 :: Expr
-- prettyExpr4 = ExprLetrec "f" (ElimList (BranchNil (ExprInt 0)) (BranchCons "x" "xs" (ExprVar "x")))
--             (ExprApp (ExprVar "f") (ExprNil))

-- prettyExpr5 :: Expr
-- prettyExpr5 = ExprMatch (ExprVar "x") (ElimPair "a" TypInt "b" TypInt (ExprAdd (ExprVar "a") (ExprVar "b")))