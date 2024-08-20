module Fluid where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
   args <- argv
   for_ args \x -> log (show x)