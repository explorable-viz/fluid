module ICFP2024 where

import Prelude

import App.Fig (drawFig, runAffs_)
import Effect (Effect)
import Test.Specs (linkedInputs_spec4, linkedInputs_spec5, linkedInputs_spec_no_sel)
import Test.Util.Suite (linkedInputsTest)

main :: Effect Unit
main = runAffs_ drawFig $ linkedInputsTest <$>
   [ linkedInputs_spec_no_sel
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
