module ICFP2024 where

import Prelude

import App.Fig (drawFig)
import App.Util (runAffs_)
import Effect (Effect)
import Test.Specs.LinkedInputs (linkedInputs_spec5)
import Test.Util.Suite (linkedInputsTest)

main :: Effect Unit
main = runAffs_ drawFig $ linkedInputsTest <$>
   [ linkedInputs_spec5
   ]
