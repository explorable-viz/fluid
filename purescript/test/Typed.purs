module Test.Typed where

import Prelude (($), Unit, show)
import Effect (Effect)
import Effect.Class.Console (log)
import Bindings (Bindings(..))
import Expr (Ctx, Expr(..))
import Typed
import Pretty (pretty)

typExpr1 :: Expr
typExpr1 = ExprLet "x" (ExprInt 5) (ExprVar "x")

ctx1 :: Ctx
ctx1 = Empty

typExpr2 :: Expr
typExpr2 = ExprLet "x" (ExprTrue) (ExprVar "z")

testTyp :: Effect Unit
testTyp = do
    let r1 = typeOf typExpr1 ctx1
    log $ show (pretty r1)
