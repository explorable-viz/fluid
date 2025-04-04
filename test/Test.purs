module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (dictVal, envVal, listElement, select, (>.>))
import Bind ((↦))
import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Module.Web (File(..), Folder(..), loadFile)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

main :: Effect Unit
main = run tests

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = linkedInputsSuite
   [ { spec:
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
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]
