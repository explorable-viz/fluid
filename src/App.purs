module App where

import Prelude hiding (absurd)

import App.Fig (FigSpec, atDivId, drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Bind ((↦))
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Test.Specs.LinkedOutputs (linkedOutputs_spec1)

fig1 :: FigSpec
fig1 =
   { file: File "slicing/convolution/emboss"
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
   { file: File "slicing/convolution/emboss-wrap"
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
   { file: File "slicing/convolution/emboss"
   , imports:
        [ "lib/convolution"
        , "example/slicing/convolution/test-image"
        , "example/slicing/convolution/filter/emboss"
        ]
   , datasets: []
   , inputs: [ "input_image" ]
   }

energyScatter :: FigSpec
energyScatter =
   { imports: []
   , datasets:
        [ "renewables" ↦ "example/linked-inputs/renewables"
        , "nonRenewables" ↦ "example/linked-inputs/non-renewables"
        ]
   , file: File "linked-inputs/energyscatter"
   , inputs: [ "renewables", "nonRenewables" ]
   }

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/lib") (File "convolution")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "renewables")
      , loadFile' (Folder "fluid/example/slicing/convolution") (File "emboss")
      ]
   runAffs_ drawFig
      [ {-atDivId "fig-4" <$> loadFig energyScatter
      , -} atDivId "fig-conv-1" <$> loadFig fig1
      , atDivId "fig-conv-2" <$> loadFig fig2
      , atDivId "fig-conv-3" <$> loadFig fig3
      , atDivId "fig-1" <$> loadFig linkedOutputs_spec1.spec
      ]
