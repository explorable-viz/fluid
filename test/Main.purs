module Test.Main where

import Prelude hiding (add)

import App.Util.Select (listElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark.Main (BenchSuite, benchmarks)
import Test.Specs (linking_cases)
import Test.Util.Many (bwdMany, linkedOutputsMany)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
main = run tests

--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdMany
   [ { file: "dtw/average-series"
     , bwd_expect_file: "dtw/average-series.expect"
     , fwd_expect: "(2.5 : (⸨0.5⸩ : (0.5 : (2.5 : (2.5 : (1.0 : (0.5 : [])))))))"
     , δv: listElement 1 neg
     }
   ]

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite) <> linkedOutputsMany linking_cases
