module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
-- import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util ((×))

main :: Effect Unit
-- main = run tests

main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "dicts"
     , imports: []
     , fwd_expect:
          "{|[\"d\"] : {||}, [\"e\"] : {|[\"a\"] : 5, [\"ab\"] : 6|}, [\"e_ab\"] : 6, [\"f\"] : {|[\"a\"] : 6, [\"ab\"] : 7|}, [\"g\"] : {|[\"a\"] : 5|}, [\"h\"] : {|[\"fst\"] : 4, [\"snd\"] : (6 : (7 : []))|}|}"
     }
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
