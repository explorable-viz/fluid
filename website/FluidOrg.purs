module Website.FluidOrg where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module.Web (loadFile', File(..), Folder(..))
import Util ((×))
import Bind ((↦))

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/plot") (File "non-renewables") ]
   runAffs_ (uncurry drawFig)
      [ ("fig" × _) <$> loadFig
           { datasets:
                [ "renewables" ↦ "dataset/renewables-new"
                , "nonRenewables" ↦ "dataset/non-renewables"
                ]
           , imports: []
           , file: File "plot/non-renewables"
           , inputs: [ "nonRenewables" ]
           }
      ]
