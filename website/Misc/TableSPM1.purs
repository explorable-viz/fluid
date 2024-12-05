module Website.Misc.TableSPM1 where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module.Web (File(..))
import Util ((×))

fig :: FigSpec
fig =
   { datasets:
        [ "tableData" ↦ "example/text-viz/explainable-table"
        , "likelihoods" ↦ "example/text-viz/likelihoods"
        ]
   , imports: [ "lib/text-viz" ]
   , file: File "text-viz/table-spm-1"
   , inputs: [ "tableData", "likelihoods" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
