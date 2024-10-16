module Website where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Standalone.NonRenewables as NonRenewables
import Util ((×))

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/slicing/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/dataset") (File "non-renewables")
      ]
   runAffs_ (uncurry drawFig)
      [ ("fig-1" × _) <$> loadFig NonRenewables.fig
      ]
