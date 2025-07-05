module Test.Specs.LinkedInputs where

import App.Util.Selector (dictVal, envVal, listElement, select, (>.>))
import Bind ((↦))
import Data.Maybe (Maybe(..))
import File (File(..), Folder(..))
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
        , linking: true
        }
   , δ_in: "nonRenewables" ↦ listElement 51 (dictVal "coalCap" select)
   , in_expect:
        envVal "nonRenewables" (listElement 51 (dictVal "coalCap" select >.> dictVal "gasCap" select >.> dictVal "nuclearCap" select >.> dictVal "petrolCap" select)) >.>
           envVal "renewables"
              ( listElement 204 (dictVal "capacity" select)
                   >.> listElement 205 (dictVal "capacity" select)
                   >.> listElement 206 (dictVal "capacity" select)
                   >.> listElement 207 (dictVal "capacity" select)
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
        , linking: true
        }
   , δ_in: "renewables" ↦ listElement 204 (dictVal "capacity" select)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 51
                ( dictVal "coalCap" select
                     >.> dictVal "gasCap" select
                     >.> dictVal "nuclearCap" select
                     >.> dictVal "petrolCap" select
                     >.> dictVal "nuclearOut" select
                )
           )
           >.> envVal "renewables"
              ( listElement 204 (dictVal "capacity" select >.> dictVal "output" select)
                   >.> listElement 205 (dictVal "capacity" select >.> dictVal "output" select)
                   >.> listElement 206 (dictVal "capacity" select >.> dictVal "output" select)
                   >.> listElement 207 (dictVal "capacity" select >.> dictVal "output" select)
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
        , linking: true
        }
   , δ_in: "nonRenewables" ↦ listElement 0 (dictVal "coalCap" select)
   , in_expect:
        envVal "nonRenewables"
           ( listElement 0
                ( dictVal "coalCap" select
                     >.> dictVal "gasCap" select
                     >.> dictVal "nuclearCap" select
                     >.> dictVal "petrolCap" select
                )
           )
           >.> envVal "renewables"
              ( listElement 0 (dictVal "capacity" select)
                   >.> listElement 1 (dictVal "capacity" select)
                   >.> listElement 2 (dictVal "capacity" select)
                   >.> listElement 3 (dictVal "capacity" select)
              )
   }

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ linkedInputs_spec3
   , linkedInputs_spec4
   , linkedInputs_spec5
   ]
