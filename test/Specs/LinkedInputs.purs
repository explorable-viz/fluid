module Test.Specs.LinkedInputs where

import Prelude

import App (energyScatter)
import App.Util.Selector (envVal, field, listElement)
import Bind ((↦))
import Lattice (neg)
import Module (File(..))
import Test.Util.Suite (TestLinkedInputsSpec)

linkedInputs_spec1 :: TestLinkedInputsSpec
linkedInputs_spec1 =
   { spec:
        { divId: "fig-1"
        , imports: []
        , datasets:
             [ "countries" ↦ "example/linked-inputs/countries"
             , "cities" ↦ "example/linked-inputs/cities"
             ]
        , file: File "linked-inputs/water"
        , inputs: [ "countries", "cities" ]
        }
   , δ_in: "countries" ↦ listElement 0 (field "farms" neg)
   , in_expect:
        envVal "countries" (listElement 0 (field "farms" neg >>> field "popMil" neg))
           >>> envVal "cities"
              ( listElement 0 (field "water" neg)
                   >>> listElement 1 (field "water" neg)
                   >>> listElement 2 (field "water" neg)
              )
   }

linkedInputs_spec2 :: TestLinkedInputsSpec
linkedInputs_spec2 =
   { spec:
        { divId: "fig-2"
        , imports: []
        , datasets:
             [ "countries" ↦ "example/linked-inputs/countries"
             , "cities" ↦ "example/linked-inputs/cities"
             ]
        , file: File "linked-inputs/water"
        , inputs: [ "countries", "cities" ]
        }
   , δ_in: "cities" ↦ listElement 3 (field "water" neg)
        >>> listElement 4 (field "water" neg)
        >>> listElement 5 (field "water" neg)
   , in_expect:
        envVal "countries" (listElement 1 (field "farms" neg >>> field "popMil" neg))
           >>> envVal "cities"
              ( listElement 3 (field "water" neg)
                   >>> listElement 4 (field "water" neg)
                   >>> listElement 5 (field "water" neg)
              )
   }

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec: energyScatter { divId = "fig-3" }
   , δ_in: "non_renewables" ↦ listElement 51 (field "coalCap" neg)
   , in_expect:
        envVal "non_renewables" (listElement 51 (field "coalCap" neg >>> field "gasCap" neg >>> field "nuclearCap" neg >>> field "petrolCap" neg)) >>>
           envVal "renewables"
              ( listElement 204 (field "capacity" neg)
                   >>> listElement 205 (field "capacity" neg)
                   >>> listElement 206 (field "capacity" neg)
                   >>> listElement 207 (field "capacity" neg)
              )
   }

linkedInputs_spec4 :: TestLinkedInputsSpec
linkedInputs_spec4 =
   { spec: energyScatter { divId = "fig-2" }
   , δ_in: "renewables" ↦ listElement 204 (field "capacity" neg)
   , in_expect:
        envVal "non_renewables"
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
        { divId: "fig-1"
        , file: File "linked-inputs/mini-energyscatter"
        , imports: []
        , datasets:
             [ "non_renewables" ↦ "example/linked-inputs/mini-non-renewables"
             , "renewables" ↦ "example/linked-inputs/mini-renewables"
             ]
        , inputs: [ "non_renewables", "renewables" ]
        }
   , δ_in: "non_renewables" ↦ listElement 0 (field "coalCap" neg)
   , in_expect:
        envVal "non_renewables"
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

linkedInputs_spec_no_sel :: TestLinkedInputsSpec
linkedInputs_spec_no_sel =
   { spec: energyScatter { divId = "fig-3" }
   , δ_in: "non_renewables" ↦ identity
   , in_expect:
        envVal "non_renewables" (identity)
           >>> envVal "renewables" (identity)
   }

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ linkedInputs_spec1
   , linkedInputs_spec2
   , linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   , linkedInputs_spec_no_sel
   ]
