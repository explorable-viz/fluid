module Test.Specs.LinkedInputs where

import App.Util (Selector)
import App.Util.Selector (eachElement, envVal, field, valOf, listElement, select, none, (>.>))
import Bind ((↦))
import Data.Maybe (Maybe(..))
import Test.Util (fluidSrcPaths)
import Test.Util.Suite (TestLinkedInputsSpec)
import DataType (ClassTable)
import Val (Val)

yearAndCountry :: ClassTable -> Selector Val
yearAndCountry classes = eachElement (field classes "year" select >.> field classes "country" select)

linkedInputs_spec3 :: TestLinkedInputsSpec
linkedInputs_spec3 =
   { spec:
        { fluidSrcPaths
        , inputs: [ "renewables", "nonRenewables" ]
        , query: false
        , ignoreInputs: \classes -> envVal "renewables" (valOf (yearAndCountry classes)) >.> envVal "nonRenewables" (valOf (yearAndCountry classes))
        , linking: true
        , rowFilter: Nothing
        }
   , δ_in: \classes -> "nonRenewables" ↦ listElement 51 (field classes "coalCap" select)
   , in_expect: \classes ->
        envVal "nonRenewables" (valOf (listElement 51 (field classes "coalCap" select >.> field classes "gasCap" select >.> field classes "nuclearCap" select >.> field classes "petrolCap" select))) >.>
           envVal "renewables"
              ( valOf
                   ( listElement 204 (field classes "capacity" select)
                        >.> listElement 205 (field classes "capacity" select)
                        >.> listElement 206 (field classes "capacity" select)
                        >.> listElement 207 (field classes "capacity" select)
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
        , ignoreInputs: \classes -> envVal "renewables" (valOf (yearAndCountry classes)) >.> envVal "nonRenewables" (valOf (yearAndCountry classes))
        , linking: true
        , rowFilter: Nothing
        }
   , δ_in: \classes -> "renewables" ↦ listElement 204 (field classes "capacity" select)
   , in_expect: \classes ->
        envVal "nonRenewables"
           ( valOf
                ( listElement 51
                     ( field classes "coalCap" select
                          >.> field classes "gasCap" select
                          >.> field classes "nuclearCap" select
                          >.> field classes "petrolCap" select
                          >.> field classes "nuclearOut" select
                     )
                )
           )
           >.> envVal "renewables"
              ( valOf
                   ( listElement 204 (field classes "capacity" select >.> field classes "output" select)
                        >.> listElement 205 (field classes "capacity" select >.> field classes "output" select)
                        >.> listElement 206 (field classes "capacity" select >.> field classes "output" select)
                        >.> listElement 207 (field classes "capacity" select >.> field classes "output" select)
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
   , δ_in: \classes -> "nonRenewables" ↦ listElement 0 (field classes "coalCap" select)
   , in_expect: \classes ->
        envVal "nonRenewables"
           ( valOf
                ( listElement 0
                     ( field classes "coalCap" select
                          >.> field classes "gasCap" select
                          >.> field classes "nuclearCap" select
                          >.> field classes "petrolCap" select
                     )
                )
           )
           >.> envVal "renewables"
              ( valOf
                   ( listElement 0 (field classes "capacity" select)
                        >.> listElement 1 (field classes "capacity" select)
                        >.> listElement 2 (field classes "capacity" select)
                        >.> listElement 3 (field classes "capacity" select)
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
