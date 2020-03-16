module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ""

ordToString :: Ordering -> String
ordToString LT = "LT"
ordToString GT = "GT"
ordToString EQ = "EQ"