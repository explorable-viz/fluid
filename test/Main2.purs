module Test.Main2 where

import Prelude
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.Spec (before, it)
import App.Demo (convolutionFig, linkingFig)
import App.Renderer (Fig)
import DataType (cCons)
import Lattice (𝔹)
import Module (File(..))
import Test.Util (Test, run, testBwd)
import Val (Val(..))

test_fig :: Aff Fig -> Test Unit
test_fig setup =
   before setup $
      it "hello" \_ -> do
         pure unit

tests :: Array (Test Unit)
tests = unsafePartial [test_fig convolutionFig, test_fig linkingFig]

main :: Effect Unit
main = void (sequence (run <$> tests))

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
   testBwd (File "section-5-example") (File "section-5-example-1.expect")
           (Constr true cCons (hole : (Constr false cCons (hole : (Constr false cCons (hole : hole : Nil)) : Nil)) : Nil))
           "(88 _:_ (6 : (4 : [])))"
]
