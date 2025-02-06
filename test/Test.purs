module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (snd)
import Bind ((↦))
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Lattice (neg)
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
scratchpad = linkedOutputsSuite
   [ { spec:
          { fluidSrcPaths: [ Folder "test/fluid", Folder "fluid" ]
          , datasets: [ "data" ↦ "linked-outputs/pairs-data" ]
          , imports: []
          , file: File "linked-outputs/pairs"
          , inputs: [ "data" ]
          }
     , δ_out: snd neg
     , out_expect: neg
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
