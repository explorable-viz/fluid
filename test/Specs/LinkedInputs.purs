module Test.Specs.LinkedInputs where

import Prelude

import App.Fig (FigSpec)
import App.Util.Selector (envVal, field, listElement)
import Bind ((↦))
import Lattice (neg)
import Module (File(..))
import Test.Util.Suite (TestLinkedInputsSpec)

energyScatter :: FigSpec
energyScatter =
   { imports: []
   , datasets:
        [ "renewables" ↦ "example/linked-inputs/renewables"
        , "nonRenewables" ↦ "example/linked-inputs/non-renewables"
        ]
   , file: File "linked-inputs/energyscatter"
   , inputs: [ "renewables", "nonRenewables" ]
   }

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec: energyScatter
   , δ_in: "nonRenewables" ↦ listElement 51 (field "coalCap" neg)
   , in_expect:
        envVal "nonRenewables" (listElement 51 (field "coalCap" neg >>> field "gasCap" neg >>> field "nuclearCap" neg >>> field "petrolCap" neg)) >>>
           envVal "renewables"
              ( listElement 204 (field "capacity" neg)
                   >>> listElement 205 (field "capacity" neg)
                   >>> listElement 206 (field "capacity" neg)
                   >>> listElement 207 (field "capacity" neg)
              )
   }

linkedInputs_spec4 :: TestLinkedInputsSpec
linkedInputs_spec4 =
   { spec: energyScatter
   , δ_in: "renewables" ↦ listElement 204 (field "capacity" neg)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 51
                ( field "coalCap" neg
                     >>> field "gasCap" neg
                     >>> field "nuclearCap" neg
                     >>> field "petrolCap" neg
                     >>> field "nuclearOut" neg
                )
           )
           >>> envVal "renewables"
              ( listElement 204 (field "capacity" neg >>> field "output" neg)
                   >>> listElement 205 (field "capacity" neg >>> field "output" neg)
                   >>> listElement 206 (field "capacity" neg >>> field "output" neg)
                   >>> listElement 207 (field "capacity" neg >>> field "output" neg)
              )
   }

linkedInputs_spec5 :: TestLinkedInputsSpec
linkedInputs_spec5 =
   { spec:
        { file: File "linked-inputs/mini-energyscatter"
        , imports: []
        , datasets:
             [ "nonRenewables" ↦ "example/linked-inputs/mini-non-renewables"
             , "renewables" ↦ "example/linked-inputs/mini-renewables"
             ]
        , inputs: [ "nonRenewables", "renewables" ]
        }
   , δ_in: "nonRenewables" ↦ listElement 0 (field "coalCap" neg)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 0
                ( field "coalCap" neg
                     >>> field "gasCap" neg
                     >>> field "nuclearCap" neg
                     >>> field "petrolCap" neg
                )
           )
           >>> envVal "renewables"
              ( listElement 0 (field "capacity" neg)
                   >>> listElement 1 (field "capacity" neg)
                   >>> listElement 2 (field "capacity" neg)
                   >>> listElement 3 (field "capacity" neg)
              )
   }

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
