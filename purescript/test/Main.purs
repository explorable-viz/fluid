module Test.Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Pretty
import Pretty (pretty, render)

main :: Effect Unit
main = do
  let res = render (pretty prettyExpr3)
  log res
