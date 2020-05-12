module Test.Eval where

import Bindings (Bindings(..))
import Expr (Elim(..), Expr(..))
import Eval (ExplVal, eval)
import Primitive (add)


expr1 :: Expr
expr1 = let e = Cons (Int 1) (Cons (Int 2) (Cons (Int 3) Nil))
        in Letrec "f" (ElimList { bnil  : Nil,
                                      bcons : { x : "x", y : "xs", e : Cons (BinaryApp add (Var "x") (Int 1))
                                                                            (App (Var "f") (Var "xs"))}})
                      (App (Var "f") e)

evalExpr1 :: Partial => ExplVal
evalExpr1 = eval Empty expr1
