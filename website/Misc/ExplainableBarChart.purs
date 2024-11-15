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
        , "barData" ↦ "example/text-viz/barchart-table"
        ]
   , imports: []
   , file: File "text-viz/explain-barchart"
   , inputs: [ "likelihoods", "barData" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
