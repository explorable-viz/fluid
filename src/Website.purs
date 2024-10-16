module Website where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Standalone.Convolution as Convolution
import Test.Specs.LinkedOutputs (linkedOutputs_spec1)
import Util ((×))

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/slicing/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/dataset") (File "renewables")
      , loadFile' (Folder "fluid/lib") (File "convolution")
      , loadFile' (Folder "fluid/example/slicing/convolution") (File "emboss-wrap")
      ]
   runAffs_ (uncurry drawFig)
      [ ("fig-conv-2" × _) <$> loadFig Convolution.fig
      , ("fig-1" × _) <$> loadFig linkedOutputs_spec1.spec
      ]
