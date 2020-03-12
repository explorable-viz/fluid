module Test.Main where

import Prelude ((<>), Unit, discard, show)
import Effect (Effect)
import Effect.Class.Console (log)
import Arithmetic (testAdd1)
import List (testList1)

main :: Effect Unit
main = do
  let addresult  = testAdd1
      listresult = testList1 
  log ("Add test 1: " <> show addresult)
  log ("List test 1: " <> show listresult)
  log "You should add some tests."

