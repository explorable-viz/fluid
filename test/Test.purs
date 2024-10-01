module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite)
import Util ((×))

main :: Effect Unit
main = run tests

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite [ { file: "dict/match", imports: [], bwd_expect_file: "dict/match.expect", δv: neg, fwd_expect: "", datasets: [] } ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
