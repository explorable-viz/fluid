module Standalone.MovingAverage where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))
import Bind ((↦))

fig :: FigSpec
fig =
   { datasets: [ "points" ↦ "example/linked-outputs/linechartpoints" ]
   , imports: []
   , file: File "/linked-outputs/movingaverage"
   , inputs: [ "points" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]