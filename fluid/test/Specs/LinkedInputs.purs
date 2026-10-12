module Test.Specs.LinkedInputs where

import App.Util (Selector)
import App.Util.Selector (eachElement, envVal, field, valOf, listElement, select, none, (>.>))
import Bind ((↦))
import Data.Maybe (Maybe(..))
import Test.Util (fluidSrcPaths)
import Test.Util.Suite (TestLinkedInputsSpec)
import DataType (FieldIndex)
import Val (Val)

yearAndCountry :: FieldIndex -> Selector Val
yearAndCountry fieldIndex = eachElement (field fieldIndex "year" select >.> field fieldIndex "country" select)

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec:
        { fluidSrcPaths
        , inputs: [ "renewables", "nonRenewables" ]
        , query: false
        , ignoreInputs: \fieldIndex -> envVal "renewables" (valOf (yearAndCountry fieldIndex)) >.> envVal "nonRenewables" (valOf (yearAndCountry fieldIndex))
        , linking: true
        , rowFilter: Nothing
        }
   , δ_in: \fieldIndex -> "nonRenewables" ↦ listElement 51 (field fieldIndex "coalCap" select)
   , in_expect: \fieldIndex ->
        envVal "nonRenewables" (valOf (listElement 51 (field fieldIndex "coalCap" select >.> field fieldIndex "gasCap" select >.> field fieldIndex "nuclearCap" select >.> field fieldIndex "petrolCap" select))) >.>
           envVal "renewables"
              ( valOf
                   ( listElement 204 (field fieldIndex "capacity" select)
                        >.> listElement 205 (field fieldIndex "capacity" select)
                        >.> listElement 206 (field fieldIndex "capacity" select)
                        >.> listElement 207 (field fieldIndex "capacity" select)
                   )
              )
   , file: "linked_inputs/energyscatter.fld"
   }

linkedInputs_spec4 :: TestLinkedInputsSpec
linkedInputs_spec4 =
   { spec:
        { fluidSrcPaths
        , inputs: [ "renewables", "nonRenewables" ]
        , query: false
        , ignoreInputs: \fieldIndex -> envVal "renewables" (valOf (yearAndCountry fieldIndex)) >.> envVal "nonRenewables" (valOf (yearAndCountry fieldIndex))
        , linking: true
        , rowFilter: Nothing
        }
   , δ_in: \fieldIndex -> "renewables" ↦ listElement 204 (field fieldIndex "capacity" select)
   , in_expect: \fieldIndex ->
        envVal "nonRenewables"
           ( valOf
                ( listElement 51
                     ( field fieldIndex "coalCap" select
                          >.> field fieldIndex "gasCap" select
                          >.> field fieldIndex "nuclearCap" select
                          >.> field fieldIndex "petrolCap" select
                          >.> field fieldIndex "nuclearOut" select
                     )
                )
           )
           >.> envVal "renewables"
              ( valOf
                   ( listElement 204 (field fieldIndex "capacity" select >.> field fieldIndex "output" select)
                        >.> listElement 205 (field fieldIndex "capacity" select >.> field fieldIndex "output" select)
                        >.> listElement 206 (field fieldIndex "capacity" select >.> field fieldIndex "output" select)
                        >.> listElement 207 (field fieldIndex "capacity" select >.> field fieldIndex "output" select)
                   )
              )
   , file: "linked_inputs/energyscatter.fld"
   }

linkedInputs_spec5 :: TestLinkedInputsSpec
linkedInputs_spec5 =
   { spec:
        { fluidSrcPaths

        , inputs: [ "nonRenewables", "renewables" ]
        , query: false
        , ignoreInputs: \_ -> none
        , linking: true
        , rowFilter: Nothing
        }
   , δ_in: \fieldIndex -> "nonRenewables" ↦ listElement 0 (field fieldIndex "coalCap" select)
   , in_expect: \fieldIndex ->
        envVal "nonRenewables"
           ( valOf
                ( listElement 0
                     ( field fieldIndex "coalCap" select
                          >.> field fieldIndex "gasCap" select
                          >.> field fieldIndex "nuclearCap" select
                          >.> field fieldIndex "petrolCap" select
                     )
                )
           )
           >.> envVal "renewables"
              ( valOf
                   ( listElement 0 (field fieldIndex "capacity" select)
                        >.> listElement 1 (field fieldIndex "capacity" select)
                        >.> listElement 2 (field fieldIndex "capacity" select)
                        >.> listElement 3 (field fieldIndex "capacity" select)
                   )
              )
   , file: "linked_inputs/mini_energyscatter.fld"
   }

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
