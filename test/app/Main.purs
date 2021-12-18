module Test.App.Main where

import Prelude
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.Spec (before, it)
import App.Demo (convolutionFig, linkingFig)
import App.Renderer (Fig)
import Test.Util (Test, run)

test_fig :: Aff Fig -> Test Unit
test_fig setup =
   before setup $
      it "hello" \_ -> do
         pure unit

tests :: Array (Test Unit)
tests = unsafePartial [test_fig convolutionFig, test_fig linkingFig]

main :: Effect Unit
main = void (sequence (run <$> tests))
