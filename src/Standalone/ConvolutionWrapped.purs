module Standalone.ConvolutionWrapped where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Data.Tuple (uncurry)
import Effect (Effect)
import Standalone.Convolution as Convolution
import Util ((×))

fig :: FigSpec
fig = Convolution.fig { inputs = [ "inputImage" ] }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
