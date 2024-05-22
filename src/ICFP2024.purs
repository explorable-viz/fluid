module ICFP2024 where

import Prelude

import App.Fig (atDivId, drawFig)
import App.Util (runAffs_)
import Effect (Effect)
import Test.Specs.LinkedInputs (linkedInputs_spec3, linkedInputs_spec4)
import Test.Specs.LinkedOutputs (linkedOutputs_spec2)
import Test.Util.Suite (linkedInputsTest, linkedOutputsTest)

main :: Effect Unit
main = runAffs_ drawFig
   [ atDivId "fig-3" <$> linkedInputsTest linkedInputs_spec3
   , atDivId "fig-2" <$> linkedInputsTest linkedInputs_spec4
   , atDivId "fig-1" <$> linkedOutputsTest linkedOutputs_spec2
   ]
