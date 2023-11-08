module PLDI2024 where

import Prelude hiding (absurd)

import App.Fig (FigSpec, LinkedOutputsFigSpec, drawFiles, drawFigs, drawLinkedOutputsFigs, loadFig, loadLinkedOutputsFig)
import Effect (Effect)
import Module (File(..), Folder(..))
import Util ((×))

-- Will changes to PLDI figures but for now same as those on f.luid.org main page.
linkedOutputsFig1 :: LinkedOutputsFigSpec
linkedOutputsFig1 =
   { divId: "fig-1"
   , file1: File "water-bar-chart"
   , file2: File "water-ratio-chart"
   , dataFile: File "water-consumption-data"
   , x: "data"
   }

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
   drawFiles [ Folder "fluid/lib" × File "convolution" ]
   drawFigs [ loadFig fig1, loadFig fig2 ]
   drawLinkedOutputsFigs [ loadLinkedOutputsFig linkedOutputsFig1 ]
