module App where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Test.Specs.LinkedInputs (energyScatter)
import Test.Specs.LinkedOutputs (linkedOutputs_spec1)
import Util ((×))
import Bind ((↦))

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
   { file: File "slicing/convolution/emboss-wrap"
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
   { datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
   , imports: []
   , file: File "text"
   , inputs: [ "renewables" ]
   }

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/graphics") (File "small-barchart")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/example/linked-outputs") (File "renewables")
      , loadFile' (Folder "fluid/lib") (File "convolution")
      , loadFile' (Folder "fluid/example/slicing/convolution") (File "emboss-wrap")
      ]
   runAffs_ (uncurry drawFig)
      [ ("fig-text" × _) <$> loadFig fig4
      , ("fig-4" × _) <$> loadFig energyScatter
      , ("fig-conv-2" × _) <$> loadFig fig2
      , ("fig-1" × _) <$> loadFig linkedOutputs_spec1.spec
      ]
