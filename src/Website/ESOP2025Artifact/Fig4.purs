module Website.Esop2025Artifact.Fig4 where

import Prelude

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..))
import Util ((×))

spec :: FigSpec
spec =
   { datasets:
        [ "renewables" ↦ "dataset/renewables-new"
        , "nonRenewables" ↦ "dataset/non-renewables"
        ]
   , imports: []
   , file: File "plot/stacked-bar-chart"
   , inputs: [ "nonRenewables" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [ ("fig" × _) <$> loadFig spec
   ]
