module App where

import Prelude hiding (absurd)

import App.Fig (FigSpec, drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Bind ((↦))
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Test.Specs.LinkedOutputs (linkedOutputs_spec1)

fig1 :: FigSpec
fig1 =
   { divId: "fig-conv-1"
   , file: File "slicing/convolution/emboss"
   , imports:
        [ "lib/convolution"
        , "example/slicing/convolution/test-image"
        , "example/slicing/convolution/filter/emboss"
        ]
   , datasets: []
   , inputs: [ "input_image", "filter" ]
   }

fig2 :: FigSpec
fig2 =
   { divId: "fig-conv-2"
   , file: File "slicing/convolution/emboss-wrap"
   , imports:
        [ "lib/convolution"
        , "example/slicing/convolution/test-image"
        , "example/slicing/convolution/filter/emboss"
        ]
   , datasets: []
   , inputs: [ "input_image", "filter" ]
   }

fig3 :: FigSpec
fig3 =
   { divId: "fig-conv-3"
   , file: File "slicing/convolution/emboss"
   , imports:
        [ "lib/convolution"
        , "example/slicing/convolution/test-image"
        , "example/slicing/convolution/filter/emboss"
        ]
   , datasets: []
   , inputs: [ "input_image" ]
   }

fig4 :: FigSpec
fig4 =
   { divId: ""
   , imports: []
   , datasets:
        [ "renewables" ↦ "example/linked-inputs/renewables"
        , "nonRenewables" ↦ "example/linked-inputs/non-renewables"
        ]
   , file: File "linked-inputs/energyscatter"
   , inputs: [ "renewables", "nonRenewables" ]
   }

energyScatter :: FigSpec
energyScatter = fig4 { divId = "fig-4" }

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/lib") (File "convolution")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "renewables")
      , loadFile' (Folder "fluid/example/slicing/convolution") (File "emboss")
      ]
   --   runAffs_ drawFig [ loadFig fig1, loadFig fig2, loadFig fig3, loadFig energyScatter ]
   runAffs_ drawFig [ loadFig linkedOutputs_spec1.spec ]
