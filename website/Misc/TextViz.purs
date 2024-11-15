module Website.Misc.TextViz where

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
   { datasets:
        [ "tableData" ↦ "example/text-viz/explainable-table"
        , "modelProbs" ↦ "example/text-viz/fake-probabilities"
        , "likelihoods" ↦ "example/text-viz/likelihoods"
        ]
   , imports: []
   , file: File "text-viz/explain-table"
   , inputs: [ "tableData", "modelProbs", "likelihoods" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
