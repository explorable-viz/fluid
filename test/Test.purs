module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (listCell, listElement, some)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util (type (×), (×))

main :: Effect Unit
--main = run tests

main = run $ asTestSuite $ suite desugar_cases

--main = run scratchpad

scratchpad :: TestSuite
scratchpad =
   asTestSuite
      ( suite
           [ { file: "lookup", imports: [], fwd_expect: "Some \"sarah\"" }
           , { file: "mergeSort", imports: [], fwd_expect: "(1 : (2 : (3 : [])))" }
           , { file: "pattern-match", imports: [], fwd_expect: "4" }
           , { file: "records", imports: [], fwd_expect: "{a : 2, b : 6, c : 7, d : (5 : []), e : 7}" }
           ]
      ) <> asTestSuite
      ( bwdSuite
           [ { file: "intersperse"
             , imports: []
             , bwd_expect_file: "intersperse-1.expect"
             , δv: listCell 1 neg
             , fwd_expect: "(1 : ⸨(0 : (2 : (0 : (3 : []))))⸩)"
             , datasets: []
             }
           , { file: "intersperse"
             , imports: []
             , bwd_expect_file: "intersperse-2.expect"
             , δv: listCell 2 neg
             , fwd_expect: "⸨(1 : (0 : ⸨(2 : (0 : (3 : [])))⸩))⸩"
             , datasets: []
             }
           , { file: "lookup"
             , imports: []
             , bwd_expect_file: "lookup.expect"
             , δv: some neg
             , fwd_expect: "⸨Some \"Germany\"⸩"
             , datasets: []
             }
           , { file: "zipWith"
             , imports: []
             , bwd_expect_file: "zipWith-1.expect"
             , δv: listElement 1 neg
             , fwd_expect: "(13.0 : (⸨25.0⸩ : (41.0 : [])))"
             , datasets: []
             }
           ]
      )

type TestSuite = Array (String × Aff Unit)

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
