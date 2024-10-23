module Website.Misc.EnergyScatter where

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
   { imports: []
   , datasets:
        [ "renewables" ↦ "dataset/renewables-new"
        , "nonRenewables" ↦ "dataset/non-renewables"
        ]
   , file: File "linked-inputs/energyscatter"
   , inputs: [ "renewables", "nonRenewables" ]
   }

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
