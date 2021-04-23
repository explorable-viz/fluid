module App.Demo where

import Prelude
import Effect.Aff (Aff)
import Module (openWithDefaultImports)

main :: Aff Unit
main = do
   _ <- openWithDefaultImports "slicing/conv-extend"
   pure unit
