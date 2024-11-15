module Test.Specs.LinkedInputs where

import Prelude

import App.Util.Selector (dictVal, envVal, listElement)
import Bind ((↦))
import Lattice (neg)
import Module (File(..))
import Website.Misc.EnergyScatter as EnergyScatter
import Test.Util.Suite (TestLinkedInputsSpec)

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec: EnergyScatter.fig
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
   { spec: EnergyScatter.fig
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
        { file: File "linked-inputs/mini-energyscatter"
        , imports: []
        , datasets:
             [ "nonRenewables" ↦ "dataset/mini-non-renewables"
             , "renewables" ↦ "dataset/mini-renewables"
             ]
        , inputs: [ "nonRenewables", "renewables" ]
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
