module Test.Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
-- import Test.Arithmetic (testAdd1, testAdd2)
-- import Test.List (testList1, testList2)
-- import Test.Pair (testPair1, testPair2)
import Test.PrettyTesting
import Pretty (pretty, render)

main :: Effect Unit
main = do
  let res = render (pretty textExpr5)
  log res
