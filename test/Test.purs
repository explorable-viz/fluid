module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Many (BenchSuite, linkedInputsSuite, linkedOutputsSuite, bwdSuite)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run $ linkedInputsSuite linkedInputs_cases
--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "motivating-example"
     , bwd_expect_file: "motivating-example.expect"
     , fwd_expect: "⸨570⸩"
     , δv: neg
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
