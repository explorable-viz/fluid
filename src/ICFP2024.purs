module ICFP2024 where

import Prelude

import App.Fig (drawFig, runAffs_)
import Effect (Effect)
import Test.Specs.LinkedInputs (linkedInputs_spec3, linkedInputs_spec4, linkedInputs_spec5)
import Test.Util.Suite (linkedInputsTest)

main :: Effect Unit
main = runAffs_ drawFig $ linkedInputsTest <$>
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
