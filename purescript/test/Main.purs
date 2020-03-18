module Test.Main where

import Prelude ((<>), Unit, bind, discard, show)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Arithmetic (testAdd1, testAdd2)
import Test.List (testList1, testList2)
import Test.Pair (testPair1, testPair2)
import Test.Parse (testParse1)

main :: Effect Unit
main = do
  let addresult_e  = testAdd1
      addresult_t  = testAdd2
      listresult_e = testList1
      listresult_t = testList2
      pairresult_e = testPair1
      pairresult_t = testPair2
  log ("Add eval    : " <> show addresult_e)
  log ("Add typeOf  : " <> show addresult_t)
  log ("List eval   : " <> show listresult_e)
  log ("List typeOf : " <> show listresult_t)
  log ("Pair eval   : " <> show pairresult_e)
  log ("Pair typeOf : " <> show pairresult_t)
  x <- testParse1
  log "Ok"
