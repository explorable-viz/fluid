module Website.FluidOrg.MovingAverage where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module.Files (File(..), Folder(..))
import Module.Web (loadFile')
import Test.Specs.LinkedOutputs (movingAverages_spec)
import Util ((×))

main :: Effect Unit
main = do
   runAffs_ drawFile [ loadFile' (Folder "fluid/example/linked-outputs") (File "moving-average") ]
   runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig movingAverages_spec.spec ]
