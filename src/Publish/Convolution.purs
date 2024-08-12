module Publish.Convolution where

import Prelude hiding (absurd)

import App.Fig (FigSpec, drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
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
   , inputs: [ "input_image", "filter" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
