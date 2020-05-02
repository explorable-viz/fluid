module Test.Fwd where

import Partial.Unsafe (unsafePartial)
import Bindings (Bindings(..))
import Expl (Expl)
import Expr (Availability(..), Elim(..), Expr(..))
import Eval (ExplVal(..), eval)
import Fwd (fwd)
import Val (Val)


e0 :: Expr
e0 = let es = Cons (Int 1) (Cons (Int 2) (Cons (Int 3) Nil))
    in Letrec "f" (ElimList { bnil  : Nil,
                              bcons : { x : "x", y : "xs", e : Cons (Add (Var "x") (Int 1))
                                                                    (App (Var "f") (Var "xs"))}})
                  (App (Var "f") es)

e :: Expr
e = let es = Cons (Int 1) (ConsSel (IntSel 2) (Cons (Int 3) Nil))
    in Letrec "f" (ElimList { bnil  : Nil,
                              bcons : { x : "x", y : "xs", e : Cons (Add (Var "x") (Int 1))
                                                                    (App (Var "f") (Var "xs"))}})
                  (App (Var "f") es)

t0 :: Expl
t0 = let ExplVal { t, v } = unsafePartial (eval Empty e0) in t

v :: Val
v = unsafePartial (fwd Empty e t0 Top)
