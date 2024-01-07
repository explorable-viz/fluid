module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.App (app_tests)
import Test.Benchmark (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "include-input-into-output"
     , imports: [ "example/lib/some-constants" ]
     , fwd_expect: "(1, 1)"
     }
   , { file: "arithmetic", imports: [], fwd_expect: "42" }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
   <> app_tests
