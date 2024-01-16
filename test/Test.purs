module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (constrArg)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import DataType (cPair)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.App (app_tests)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases, linkedOutputs_cases2)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, linkedOutputsSuite2)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run $ linkedOutputsSuite2 linkedOutputs_cases2

--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "pairs"
     , imports: []
     , bwd_expect_file: "pairs.expect"
     , fwd_expect: "((⸨3⸩, 4), 7)"
     , δv: constrArg cPair 0 (constrArg cPair 0 neg)
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedOutputsSuite2 linkedOutputs_cases2
   <> linkedInputsSuite linkedInputs_cases
   <> app_tests
