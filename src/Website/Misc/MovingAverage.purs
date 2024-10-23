module Website.Misc.MovingAverage where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs.LinkedOutputs (movingAverages_spec)
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [ ("fig" × _) <$> loadFig movingAverages_spec.spec
   ]
