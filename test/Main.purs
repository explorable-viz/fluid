module Test.Main where

import Prelude hiding (add)

import App.Util.Select (listElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark.Util (BenchRow)
import Test.Specs (bwd_cases, desugar_cases, graphics_cases, linking_cases, misc_cases)
import Test.Util.Many (bwdMany, linkedOutputsMany, many, withDatasetMany)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
main = run tests
--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdMany
   [ { file: "dtw/compute-dtw"
     , bwd_expect_file: "dtw/compute-dtw.expect"
     , fwd_expect: "((1, 1) : (⸨(⸨2⸩, ⸨2⸩)⸩ : ((2, 3) : ((3, 4) : ((4, 5) : ((5, 6) : ((5, 7) : [])))))))"
     , δv: listElement 1 neg
     }
   ]

type BenchSuite = (Int × Boolean) -> Array (String × Aff BenchRow)
type TestSuite = Array (String × Aff Unit)

benchmarks :: Array BenchSuite
benchmarks =
   [ many desugar_cases
   , many misc_cases
   , bwdMany bwd_cases
   , withDatasetMany graphics_cases
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite) <> linkedOutputsMany linking_cases
