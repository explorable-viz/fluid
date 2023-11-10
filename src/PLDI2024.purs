module PLDI2024 where

import Prelude
import App.Fig (LinkedOutputsFigSpec, drawLinkedOutputsFigs, loadLinkedOutputsFig)
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

main :: Effect Unit
main = do
   drawLinkedOutputsFigs [ loadLinkedOutputsFig waterFig ]
