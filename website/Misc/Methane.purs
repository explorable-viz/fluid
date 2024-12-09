module Website.Misc.Methane where

import Prelude

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))

figSpec :: FigSpec
figSpec =
   { datasets: [ "methane" ↦ "dataset/methane-emissions" ]
   , imports: []
   , file: File "plot/methane"
   , inputs: [ "methane" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig figSpec ]
