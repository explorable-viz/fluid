module Website.FluidOrg.Convolution where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Util ((×))

fig :: FigSpec
fig =
   { file: File "slicing/convolution/emboss"
   , imports:
        [ "lib/convolution"
        , "example/slicing/convolution/test-image"
        , "example/slicing/convolution/filter/emboss"
        ]
   , datasets: []
   , inputs: [ "inputImage", "filter" ]
   }

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/lib") (File "convolution")
      , loadFile' (Folder "fluid/example/slicing/convolution") (File "emboss")
      ]
   runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
