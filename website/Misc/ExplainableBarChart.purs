module Website.Misc.ExplainableBarChart where

import Prelude

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
   { datasets:
        [ "likelihoods" ↦ "example/text-viz/likelihoods"
        , "leftBarData" ↦ "example/text-viz/left-barchart-table"
        , "rightBarData" ↦ "example/text-viz/right-barchart-table"
        ]
   , imports: [ "lib/text-viz" ]
   , file: File "text-viz/figure-spm-4"
   , inputs: [ "likelihoods", "leftBarData", "rightBarData" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
