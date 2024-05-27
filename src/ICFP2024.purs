module ICFP2024 where

import Prelude

import App.Fig (drawFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs.LinkedInputs (linkedInputs_spec3, linkedInputs_spec4)
import Test.Specs.LinkedOutputs (linkedOutputs_spec2)
import Test.Util.Suite (linkedInputsTest, linkedOutputsTest)
import Util ((×))

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [ ("fig-3" × _) <$> linkedInputsTest linkedInputs_spec3
   , ("fig-2" × _) <$> linkedInputsTest linkedInputs_spec4
   , ("fig-1" × _) <$> linkedOutputsTest linkedOutputs_spec2
   ]
