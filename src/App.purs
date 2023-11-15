module App where

import Prelude hiding (absurd)

import App.Fig (FigSpec, drawFig', drawFile, drawLinkedOutputsFig', loadFig, loadLinkedOutputsFig, runAffs_)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Test.Specs (linkedOutputs_spec1)

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
   runAffs_ drawFile [ loadFile' (Folder "fluid/lib") (File "convolution") ]
   runAffs_ drawFig' [ loadFig fig1, loadFig fig2 ]
   runAffs_ drawLinkedOutputsFig' [ loadLinkedOutputsFig linkedOutputs_spec1.spec ]
