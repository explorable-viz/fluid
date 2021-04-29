module App.Demo where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import App.Renderer (drawMatrix)
import Module (openWithDefaultImports)
import Pretty (prettyP)
import Test.Util (desugarEval)
import Util ((×))

main :: Effect Unit
main =
   flip runAff_ (openWithDefaultImports "slicing/conv-extend") \result ->
   case result of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> case desugarEval ρ s of
         Left msg -> log ("Execution failed: " <> msg)
         Right (_ × v) -> do
            log (prettyP v)
            drawMatrix 5 5
