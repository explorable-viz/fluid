module Website.Misc.Renewables where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))

-- Delete this test case once we can enable/disable outputs via the UI

figSpec :: FigSpec
figSpec =
   { datasets: [ "renewables" ↦ "dataset/renewables" ]
   , imports: []
   , file: File "linked-outputs/line-chart"
   , inputs: [ "renewables" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig figSpec ]
