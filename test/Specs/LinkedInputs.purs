module Test.Specs.LinkedInputs where

import Prelude

import App.Util.Selector (dictVal, envVal, listElement)
import Bind ((↦))
import Data.Maybe (Maybe(..))
import Lattice (neg)
import Module.Web (File(..), Folder(..))
import Test.Util.Suite (TestLinkedInputsSpec)

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , imports: []
        , datasets:
             [ "renewables" ↦ "dataset/renewables-new"
             , "nonRenewables" ↦ "dataset/non-renewables"
             ]
        , file: File "linked-inputs/energyscatter"
        , inputs: [ "renewables", "nonRenewables" ]
        , query: Nothing
        }
   , δ_in: "nonRenewables" ↦ listElement 51 (dictVal "coalCap" neg)
   , in_expect:
        envVal "nonRenewables" (listElement 51 (dictVal "coalCap" neg >>> dictVal "gasCap" neg >>> dictVal "nuclearCap" neg >>> dictVal "petrolCap" neg)) >>>
           envVal "renewables"
              ( listElement 204 (dictVal "capacity" neg)
                   >>> listElement 205 (dictVal "capacity" neg)
                   >>> listElement 206 (dictVal "capacity" neg)
                   >>> listElement 207 (dictVal "capacity" neg)
              )
   }

linkedInputs_spec4 :: TestLinkedInputsSpec
linkedInputs_spec4 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , imports: []
        , datasets:
             [ "renewables" ↦ "dataset/renewables-new"
             , "nonRenewables" ↦ "dataset/non-renewables"
             ]
        , file: File "linked-inputs/energyscatter"
        , inputs: [ "renewables", "nonRenewables" ]
        , query: Nothing
        }
   , δ_in: "renewables" ↦ listElement 204 (dictVal "capacity" neg)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 51
                ( dictVal "coalCap" neg
                     >>> dictVal "gasCap" neg
                     >>> dictVal "nuclearCap" neg
                     >>> dictVal "petrolCap" neg
                     >>> dictVal "nuclearOut" neg
                )
           )
           >>> envVal "renewables"
              ( listElement 204 (dictVal "capacity" neg >>> dictVal "output" neg)
                   >>> listElement 205 (dictVal "capacity" neg >>> dictVal "output" neg)
                   >>> listElement 206 (dictVal "capacity" neg >>> dictVal "output" neg)
                   >>> listElement 207 (dictVal "capacity" neg >>> dictVal "output" neg)
              )
   }

linkedInputs_spec5 :: TestLinkedInputsSpec
linkedInputs_spec5 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , file: File "linked-inputs/mini-energyscatter"
        , imports: []
        , datasets:
             [ "nonRenewables" ↦ "dataset/mini-non-renewables"
             , "renewables" ↦ "dataset/mini-renewables"
             ]
        , inputs: [ "nonRenewables", "renewables" ]
        , query: Nothing
        }
   , δ_in: "nonRenewables" ↦ listElement 0 (dictVal "coalCap" neg)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 0
                ( dictVal "coalCap" neg
                     >>> dictVal "gasCap" neg
                     >>> dictVal "nuclearCap" neg
                     >>> dictVal "petrolCap" neg
                )
           )
           >>> envVal "renewables"
              ( listElement 0 (dictVal "capacity" neg)
                   >>> listElement 1 (dictVal "capacity" neg)
                   >>> listElement 2 (dictVal "capacity" neg)
                   >>> listElement 3 (dictVal "capacity" neg)
              )
   }

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
