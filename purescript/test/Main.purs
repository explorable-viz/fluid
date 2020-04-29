module Test.Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Eval (evalExpr1)
import Pretty (pretty)
import Data.Show (show)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  let res = show (pretty  (unsafePartial evalExpr1))
  log res
