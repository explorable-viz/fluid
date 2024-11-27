module Website.Misc.FigureSPM4b where

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
        , "ssp119Source" ↦ "example/text-viz/left-barchart-table"
        , "ssp245Source" ↦ "example/text-viz/right-barchart-table"
        ]
   , imports: [ "lib/text-viz" ]
   , file: File "text-viz/figure-spm-4"
   , inputs: [ "likelihoods", "ssp119Source", "ssp245Source" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
