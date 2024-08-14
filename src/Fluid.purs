module Fluid where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
   args <- argv
   log $ show args