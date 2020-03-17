module Test.Main where

import Prelude ((<>), Unit, bind, discard, show)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Arithmetic (testAdd1)
import Test.List (testList1)
import Test.Parse (testParse1)

main :: Effect Unit
main = do
  let addresult  = testAdd1
      listresult = testList1
  log ("Add test 1: " <> show addresult)
  log ("List test 1: " <> show listresult)
  x <- testParse1
  log "Ok"
