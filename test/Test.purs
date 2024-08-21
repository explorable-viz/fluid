module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util ((×))

main :: Effect Unit
main = run tests

--main = run $ asTestSuite $ suite desugar_cases
--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "desugar/list-comp-8", imports: [], fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-9", imports: [], fwd_expect: "(10 : (19 : []))" }
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
