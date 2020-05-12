module Test.Main where

import Prelude
import Effect (Effect)
import Test.Parse (testParse1)

main :: Effect Unit
main = do
  testParse1
