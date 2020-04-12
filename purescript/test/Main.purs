module Test.Main where

import Prelude ((<>), Unit, bind, discard, show)
import Effect (Effect)
import Effect.Class.Console (log)
-- import Test.Arithmetic (testAdd1, testAdd2)
-- import Test.List (testList1, testList2)
-- import Test.Pair (testPair1, testPair2)
import Test.Parse (testParse1)
import Test.PrettyTesting
import Pretty

main :: Effect Unit
main = do
  let res = render (pretty textExpr5)
  log res
