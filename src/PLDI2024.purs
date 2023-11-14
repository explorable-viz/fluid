module PLDI2024 where

import Prelude

import App.Fig (LinkedOutputsFigSpec, drawLinkedInputsFigs, loadLinkedInputsFig)
import Effect (Effect)
import Module (File(..))
import Test.Specs (linkedInputs_spec1)

-- Currently unused; delete once we support linked outputs/inputs in same example.
linkedOutputs :: LinkedOutputsFigSpec
linkedOutputs =
   { divId: "fig-1"
   , file1: File "water-bar-chart"
   , file2: File "water-ratio-chart"
   , dataFile: File "water-consumption-data"
   , x: "data"
   }

main :: Effect Unit
main = do
   -- drawLinkedOutputsFigs [ loadLinkedOutputsFig waterFig ]
   drawLinkedInputsFigs [ loadLinkedInputsFig linkedInputs_spec1.spec ]
