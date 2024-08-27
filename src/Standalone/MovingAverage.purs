module Standalone.MovingAverage where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))

fig :: FigSpec
fig =
   { datasets: [ "points" ↦ "example/linked-outputs/moving-average-data" ]
   , imports: []
   , file: File "/linked-outputs/moving-average"
   , inputs: [ "points" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
