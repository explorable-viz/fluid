module PLDI2024 where

import Prelude hiding (absurd)

import App.Fig (LinkedOutputsFigSpec, drawLinkedOutputsFigs, loadLinkedOutputsFig)
import Effect (Effect)
import Module (File(..))

-- Will changes to PLDI figures but for now same as those on f.luid.org main page.
linkedOutputsFig1 :: LinkedOutputsFigSpec
linkedOutputsFig1 =
   { divId: "fig-1"
   , file1: File "bar-chart"
   , file2: File "line-chart"
   , dataFile: File "renewables"
   , x: "data"
   }

main :: Effect Unit
main = do
   drawLinkedOutputsFigs [ loadLinkedOutputsFig linkedOutputsFig1 ]
