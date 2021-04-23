module App.Demo where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Module (openWithDefaultImports)

main :: Effect Unit
main = launchAff_ (openWithDefaultImports "slicing/conv-extend")
