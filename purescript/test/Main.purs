module Test.Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Pretty
import Test.Eval
import Pretty (pretty, render)
import Data.Show 
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  let res = show (pretty  (unsafePartial evalExpr1))
  log res
