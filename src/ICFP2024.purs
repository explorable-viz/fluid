module ICFP2024 where

import Prelude

import App.Fig (drawFig, runAffs_)
import Effect (Effect)
import Test.Specs (linkedInputs_spec3, linkedInputs_spec4, linkedInputs_spec5)
import Test.Util.Suite (linkedInputsTest2)

main :: Effect Unit
main = runAffs_ drawFig $ linkedInputsTest2 <$>
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
