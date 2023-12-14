module ICFP2024 where

import Prelude

import App.Fig (drawLinkedInputsFig, runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs (linkedInputs_spec3, linkedInputs_spec4, linkedInputs_spec5)
import Test.Util.Suite (loadLinkedInputsTest)

main :: Effect Unit
main =
   runAffs_ (uncurry drawLinkedInputsFig)
      [ loadLinkedInputsTest linkedInputs_spec5
      , loadLinkedInputsTest linkedInputs_spec3
      , loadLinkedInputsTest linkedInputs_spec4
      ]
