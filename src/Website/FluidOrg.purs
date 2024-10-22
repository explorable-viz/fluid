module Website.FluidOrg where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Standalone.EnergyScatter as EnergyScatter
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [ ("fig-4" × _) <$> loadFig EnergyScatter.fig
   ]
