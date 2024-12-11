module Website.FluidOrg where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module.Web (loadFile', File(..), Folder(..))
import Util ((×))
import Website.Misc.NonRenewables as NonRenewables

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/plot") (File "non-renewables") ]
   runAffs_ (uncurry drawFig)
      [ ("fig" × _) <$> loadFig NonRenewables.fig ]
