module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Eval (evalExpr1)
import Test.Fwd (v)
import Pretty (pretty)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  log $ show (pretty (unsafePartial evalExpr1))
  log $ show (pretty v)
