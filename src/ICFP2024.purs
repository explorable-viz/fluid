module ICFP2024 where

import Prelude

import App.Fig (drawLinkedInputsFig, runAffs_)
-- import App.Util.Select (field, listElement)
-- import Data.Either (Either(..))
-- import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Effect (Effect)
-- import Lattice (neg)
-- import Module (File(..))
import Test.Specs (linkedInputs_spec1, linkedInputs_spec3, linkedInputs_spec4)
import Test.Util.Suite (loadLinkedInputsTest)

main :: Effect Unit
main =
   runAffs_ (uncurry drawLinkedInputsFig)
      [ loadLinkedInputsTest linkedInputs_spec1
      , loadLinkedInputsTest linkedInputs_spec3
      , loadLinkedInputsTest linkedInputs_spec4
      ]
