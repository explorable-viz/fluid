module Standalone.ExplainableBarchart where

import Prelude

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))

fig :: FigSpec
fig =
   { datasets: []
   , imports: []
   , file: File "text-viz/explain-barchart"
   , inputs: []
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]