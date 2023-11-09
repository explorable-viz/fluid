module PLDI2024 where

import Prelude
import App.Fig (LinkedOutputsFigSpec, drawLinkedOutputsFigs, loadLinkedOutputsFig, FigSpec)
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
--   { divId: "fig-1"
--   , file1: File "bar-chart"
--   , file2: File "line-chart"
--   , dataFile: File "renewables"
--   , x: "data"
--   }

fig1 :: FigSpec
fig1 =
   { divId: "fig-conv-1"
   , file: File "slicing/convolution/emboss"
   , xs: [ "image", "filter" ]
   }

fig2 :: FigSpec
fig2 =
   { divId: "fig-conv-2"
   , file: File "slicing/convolution/emboss-wrap"
   , xs: [ "image", "filter" ]
   }

main :: Effect Unit
main = do
   drawLinkedOutputsFigs [ loadLinkedOutputsFig waterFig ]
