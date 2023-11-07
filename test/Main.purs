module Test.Main where

import Prelude hiding (add)

import App.Util.Select (listElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark.Main (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Many (BenchSuite, bwdMany, linkedInputsMany, linkedOutputsMany)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
main = run tests
--main = run $ linkedInputsMany linkedInputs_cases
--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdMany
   [ { file: "dtw/average-series"
     , bwd_expect_file: "dtw/average-series.expect"
     , fwd_expect: "(2.5 : (0.5 : (⸨0.5⸩ : (2.5 : (2.5 : (1.0 : (0.5 : [])))))))"
     , δv: listElement 2 neg
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsMany linkedOutputs_cases
   <> linkedInputsMany linkedInputs_cases
