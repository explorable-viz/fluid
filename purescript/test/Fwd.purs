module Test.Fwd where

import Partial.Unsafe (unsafePartial)
import Bindings (Bindings(..))
import Expl (Expl)
import Expr (Availability(..), Elim(..), Expr(..))
import Eval (ExplVal(..), eval)
import Fwd (fwd)
import Val (Val)


e0 :: Expr
e0 = let es = ExprCons (ExprInt 1) (ExprCons (ExprInt 2) (ExprCons (ExprInt 3) ExprNil))
    in ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                  bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                            (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                  (ExprApp (ExprVar "f") es)

e :: Expr
e = let es = ExprCons (ExprInt 1) (ExprConsSel (ExprIntSel 2) (ExprCons (ExprInt 3) ExprNil))
    in ExprLetrec "f" (ElimList { bnil  : ExprNil,
                                  bcons : { x : "x", y : "xs", e : ExprCons (ExprAdd (ExprVar "x") (ExprInt 1))
                                                                            (ExprApp (ExprVar "f") (ExprVar "xs"))}})
                  (ExprApp (ExprVar "f") es)

t0 :: Expl
t0 = let ExplVal { t, v } = unsafePartial (eval Empty e0) in t

v :: Val
v = unsafePartial (fwd Empty e t0 Top)
