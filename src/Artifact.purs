module Artifact where

import Prelude

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs.LinkedInputs (energyScatter)
import Test.Specs.LinkedOutputs (linkedOutputs_spec2)
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [ ("fig-1" × _) <$> loadFig energyScatter
   , ("fig-4" × _) <$> loadFig (linkedOutputs_spec2.spec { inputs = [ "renewables", "nonRenewables" ] })
   ]
