module PLDI2024 where

import Prelude

import App.Fig (LinkedOutputsFigSpec, LinkedInputsFig, drawLinkedInputsFig, loadLinkedInputsFig, runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Test.Specs (linkedInputs_spec1, linkedInputs_spec2)
import Test.Util (Selector)
import Test.Util.Many (TestLinkedInputsSpec)
import Util (type (×), (×), type (+), AffError)
import Val (Val)

-- Currently unused; delete once we support linked outputs/inputs in same example.
linkedOutputs :: LinkedOutputsFigSpec
linkedOutputs =
   { divId: "fig-1"
   , file1: File "water-bar-chart"
   , file2: File "water-ratio-chart"
   , dataFile: File "water-consumption-data"
   , x: "data"
   }

loadLinkedInputsTest :: forall m. TestLinkedInputsSpec -> AffError m (LinkedInputsFig × (Selector Val + Selector Val))
loadLinkedInputsTest { spec, δv } = (_ × δv) <$> loadLinkedInputsFig spec

main :: Effect Unit
main =
   runAffs_ (uncurry drawLinkedInputsFig)
      [ loadLinkedInputsTest linkedInputs_spec1
      , loadLinkedInputsTest linkedInputs_spec2
      ]
