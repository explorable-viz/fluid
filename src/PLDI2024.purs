module PLDI2024 where

import Prelude

import App.Fig (LinkedInputsFigSpec, LinkedOutputsFigSpec, drawLinkedInputsFigs, loadLinkedInputsFig)
import Effect (Effect)
import Module (File(..))

waterFig :: LinkedOutputsFigSpec
waterFig =
   { divId: "fig-1"
   , file1: File "water-bar-chart"
   , file2: File "water-ratio-chart"
   , dataFile: File "water-consumption-data"
   , x: "data"
   }

waterInFig :: LinkedInputsFigSpec
waterInFig =
   { divId: "fig-1"
   , x2: "cities"
   , x1: "countries"
   , file: File "water"
   }

main :: Effect Unit
main = do
   -- drawLinkedOutputsFigs [ loadLinkedOutputsFig waterFig ]
   drawLinkedInputsFigs [ loadLinkedInputsFig waterInFig ]
