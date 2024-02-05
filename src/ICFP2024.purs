module ICFP2024 where

import Prelude

import App.Fig (drawFig)
import App.Util (runAffs_)
import Effect (Effect)
import Test.Specs.LinkedInputs (linkedInputs_spec3, linkedInputs_spec4)
import Test.Specs.LinkedOutputs (linkedOutputs_spec2)
import Test.Util.Suite (linkedInputsTest, linkedOutputsTest)

main :: Effect Unit
main = runAffs_ drawFig $
   ( linkedInputsTest <$>
        [ linkedInputs_spec3
        , linkedInputs_spec4
        ]
   ) <>
      ( linkedOutputsTest <$>
           [ linkedOutputs_spec2
           ]
      )
