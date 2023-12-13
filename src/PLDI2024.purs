module PLDI2024 where

import Prelude

import App.Fig (drawLinkedInputsFig, runAffs_)
import App.Util.Select (field, listElement)
import Data.Either (Either(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Lattice (neg)
import Module (File(..))
import Test.Specs (linkedInputs_spec1)
import Test.Util.Suite (loadLinkedInputsTest, TestLinkedInputsSpec)

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec:
        { divId: "fig-3"
        , file: File "energy"
        , x2: "renewables"
        , x2File: File "renewables"
        , x1: "non_renewables"
        , x1File: File "non-renewables"
        }
   , δv: Left $ listElement 27 (field "nuclear" neg >>> field "petrol" neg >>> field "gas" neg >>> field "coal" neg >>> field "gdpPerCap" neg >>> field "carbonInt" neg)
   , v'_expect: "" -- No expected value due to the size of the list
   }

linkedInputs_spec4 :: TestLinkedInputsSpec
linkedInputs_spec4 =
   { spec:
        { divId: "fig-2"
        , file: File "energy"
        , x1: "non_renewables"
        , x1File: File "non-renewables"
        , x2: "renewables"
        , x2File: File "renewables"
        }
   , δv: Left $ listElement 27 (field "nuclear" neg)
   , v'_expect: ""
   }

main :: Effect Unit
main =
   runAffs_ (uncurry drawLinkedInputsFig)
      [ loadLinkedInputsTest linkedInputs_spec1
      , loadLinkedInputsTest linkedInputs_spec3
      , loadLinkedInputsTest linkedInputs_spec4
      ]
