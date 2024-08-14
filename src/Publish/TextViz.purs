module Publish.TextViz where

import Prelude hiding (absurd)

import App.Fig (FigSpec, drawFig, loadFig)
import App.Util (runAffs_)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Util ((×))
import Module (File(..))

fig :: FigSpec
fig = 
   { datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
   , imports: []
   , file: File "text"
   , inputs: [ "renewables" ]
   }


main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig]
