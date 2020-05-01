module Test.Fwd where

import Expl (Expl)
import Expr (Availability(..), Bindings(..), Elim(..), Expr(..), Val)
import Eval (ExplVal(..), eval)
import Fwd (fwd)
import Project (project)
import Partial.Unsafe (unsafePartial)


e0 :: Expr
e0 = let es = ExprCons (ExprInt 1) (ExprCons (ExprInt 2) (ExprCons (ExprInt 3) ExprNil))
    in ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                  bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                            (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                  (ExprApp (ExprVar "f") es)

e :: Expr
e = let es = ExprCons (ExprInt 1) (ExprConsTail (ExprCons (ExprInt 3) ExprNil))
    in ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                  bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                            (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                  (ExprApp (ExprVar "f") es)

t0 :: Expl
t0 = let ExplVal { t, v } = unsafePartial (eval Empty e0) in t

e' :: Expr
e' = project e

v :: Val
v = project (unsafePartial (fwd Empty e t0 Top))

v' :: Val
v' = let ExplVal { t, v } = unsafePartial (eval Empty e') in v
