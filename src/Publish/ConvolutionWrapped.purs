module Publish.ConvolutionWrapped where

import Prelude hiding (absurd)

import App (fig2)
import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig2 ]
