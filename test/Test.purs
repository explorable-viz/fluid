module Test.Test where

import Prelude hiding (add)

import App.Util.Select (listCell, matrixElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.App (app_tests)
import Test.Benchmark (benchmarks)
import Test.Specs (linkedInputs_cases, linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite
   ( bwdSuite
        [ { file: "filter"
          , imports: []
          , bwd_expect_file: "filter.expect"
          , δv: listCell 0 neg
          , fwd_expect: "⸨(⸨8⸩ : (7 : []))⸩"
          }
        , { file: "matrix-update"
          , imports: []
          , bwd_expect_file: "matrix-update.expect"
          , fwd_expect:
               "15, 13, 6, 9, 16,\n\
               \12, ⸨4000⸩, 15, 4, 13,\n\
               \14, 9, 20, 8, 1,\n\
               \4, 10, 3, 7, 19,\n\
               \3, 11, 15, 2, 9"
          , δv: matrixElement 2 2 neg
          }
        ]
        <> suite
           [ { file: "mergeSort", imports: [], fwd_expect: "(1 : (2 : (3 : [])))" }
           , { file: "nub", imports: [], fwd_expect: "(1 : (2 : (3 : (4 : []))))" }
           , { file: "range", imports: [], fwd_expect: "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))" }
           ]
        <> withDatasetSuite
           [ { imports: [ "lib/graphics" ], dataset: "dataset/renewables-restricted", file: "graphics/line-chart" }
           ]
   )

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
   <> app_tests
