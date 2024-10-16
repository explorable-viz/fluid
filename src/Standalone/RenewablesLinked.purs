module Standalone.RenewablesLinked where

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
   { datasets: [ "renewables" ↦ "dataset/renewables" ]
   , imports: []
   , file: File "slicing/linked-outputs/bar-chart-line-chart"
   , inputs: [ "renewables" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
