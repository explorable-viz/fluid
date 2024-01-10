module Test.Test where

import Prelude hiding (add)

import App.Util.Select (constrArg)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import DataType (cPair)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.App (app_tests)
import Test.Benchmark (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "output-not-source"
     , imports: []
     , bwd_expect_file: "output-not-source.expect"
     , fwd_expect: "(⸨3⸩, True)"
     , δv: constrArg cPair 0 neg
     }
   ]

scratchpad2 :: TestSuite
scratchpad2 = asTestSuite $ suite
   [ { file: "nub", imports: [], fwd_expect: "(1 : (2 : (3 : (4 : []))))" }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
   <> app_tests
