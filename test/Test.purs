module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

--main = run $ linkedOutputsSuite linkedOutputs_cases
main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "desugar/list-comp-8", imports: [], fwd_expect: "(5 : (4 : (3 : [])))" }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
