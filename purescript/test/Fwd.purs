module Test.Fwd where

import Expr (Elim(..), Expr(..))
import Project (project)


expr1 :: Expr
expr1 = let e = ExprCons (ExprInt 1) (ExprConsTail (ExprCons (ExprInt 3) ExprNil))
        in ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                      bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                                (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                      (ExprApp (ExprVar "f") e)

expr1' :: Expr
expr1' = project expr1
