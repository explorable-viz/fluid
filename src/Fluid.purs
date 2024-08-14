module Fluid where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Process as NProcess

main :: Effect Unit
main = do
   log (NProcess.version)