module Standalone.EnergyScatter where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs.LinkedInputs (energyScatter)
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig energyScatter ]
