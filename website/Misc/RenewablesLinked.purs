module Website.Misc.RenewablesLinked where

import Prelude hiding (absurd)

import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Module (File(..), Folder(..), loadFile')
import Util ((×))

fig :: FigSpec
fig =
   { datasets: [ "renewables" ↦ "dataset/renewables" ]
   , imports: []
   , file: File "slicing/linked-outputs/bar-chart-line-chart"
   , inputs: [ "renewables" ]
   }

main :: Effect Unit
main = do
   runAffs_ drawFile
      [ loadFile' (Folder "fluid/example/slicing/linked-outputs") (File "bar-chart-line-chart")
      , loadFile' (Folder "fluid/dataset") (File "renewables")
      ]
   runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig fig ]
