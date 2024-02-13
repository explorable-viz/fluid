module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (fst)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite)
import Util (type (×), (×))

main :: Effect Unit
main = run tests

--main = run $ linkedOutputsSuite linkedOutputs_cases
--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "pairs"
     , imports: []
     , bwd_expect_file: "pairs.expect"
     , fwd_expect: "((⸨3⸩, 4), 7)"
     , δv: fst (fst neg)
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
