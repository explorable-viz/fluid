module Test.Pretty where

import Expr (Expr(..))

prettyExpr1 :: Expr
prettyExpr1 = ExprLet "x" (ExprInt 5) (ExprAdd (ExprVar "x") (ExprInt 3))

prettyExpr2 :: Expr
prettyExpr2 = ExprLet "x" (ExprInt 5) (ExprLet "y" (ExprInt 3) (ExprAdd (ExprVar "x") (ExprVar "y")))

prettyExpr3 :: Expr
prettyExpr3 = ExprCons (ExprInt 5) (ExprCons (ExprInt 4) (ExprCons (ExprInt 3) ExprNil))

-- prettyExpr4 :: Expr
-- prettyExpr4 = ExprLetrec "f" (ElimList (BranchNil (ExprInt 0)) (BranchCons "x" "xs" (ExprVar "x")))
--             (ExprApp (ExprVar "f") (ExprNil))

-- prettyExpr5 :: Expr
-- prettyExpr5 = ExprMatch (ExprVar "x") (ElimPair "a" TypInt "b" TypInt (ExprAdd (ExprVar "a") (ExprVar "b")))