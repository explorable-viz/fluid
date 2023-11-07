module Test.Main where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Benchmark.Main (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Many (BenchSuite, linkedInputsMany, linkedOutputsMany)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
-- main = run $ linkedInputsMany linkedInputs_cases

main = run scratchpad

scratchpad :: TestSuite
scratchpad = tests

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsMany linkedOutputs_cases
   <> linkedInputsMany linkedInputs_cases
