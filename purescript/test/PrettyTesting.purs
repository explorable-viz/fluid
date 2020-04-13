module Test.PrettyTesting where

import Expr (Elim(..), Expr(..), Typ(..))

textExpr1 :: Expr
textExpr1 = ExprLet "x" (ExprInt 5) (ExprAdd (ExprVar "x") (ExprInt 3))

textExpr2 :: Expr
textExpr2 = ExprLet "x" (ExprInt 5) (ExprLet "y" (ExprInt 3) (ExprAdd (ExprVar "x") (ExprVar "y")))

textExpr3 :: Expr
textExpr3 = ExprCons (ExprInt 5) (ExprCons (ExprInt 4) (ExprCons (ExprInt 3) ExprNil))

textExpr4 :: Expr
textExpr4 = ExprLetrec "f" (ElimList { bnil: ExprInt 0, bcons: { x: "x", y: "xs", e: ExprVar "x" } })
            (ExprApp (ExprVar "f") (ExprNil))

textExpr5 :: Expr
textExpr5 = ExprMatch (ExprVar "x") (ElimPair { x: "a", tx: TypInt, y: "b", ty: TypInt, e: ExprAdd (ExprVar "a") (ExprVar "b") })
