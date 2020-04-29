module Test.Eval where

import Expr (Bindings(..), Elim(..), Expr(..), Val)
import Eval (ExplVal, eval)


expr1 :: Expr
expr1 = let e = (ExprCons (ExprInt 1) (ExprCons (ExprInt 2) (ExprCons (ExprInt 3) ExprNil)))
            f = ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                          bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                                    (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                                                                                    (ExprApp (ExprVar "f") e)
        in  f


env1 :: Bindings Val
env1 = Empty

evalExpr1 :: Partial => ExplVal
evalExpr1 = eval Empty expr1
