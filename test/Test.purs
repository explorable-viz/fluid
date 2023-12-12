module Test.Test where

import Prelude hiding (add)

import App.Util.Select (constr)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import DataType (cSome)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.App (app_tests)
import Test.Benchmark (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite)
import Util (type (×), (×))

main :: Effect Unit
main = run tests

--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "lookup"
     , imports: []
     , bwd_expect_file: "lookup.expect"
     , δv: constr cSome neg
     , fwd_expect: "⸨Some \"Germany\"⸩"
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
   <> app_tests
