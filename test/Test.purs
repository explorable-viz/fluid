module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (dict, dictKey, dictVal, listCell)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Lattice (neg)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util ((×))
import Website.Benchmark (benchmarks)

main :: Effect Unit
main = run tests

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ suite [ { file: "records", imports: [], fwd_expect: "{[\"a\"] : 2, [\"b\"] : 6, [\"c\"] : 7, [\"d\"] : (5 : []), [\"e\"] : 7}" } ]
   <> bwdSuite
      [ { file: "dict/create"
        , imports: []
        , bwd_expect_file: "dict/create.expect"
        , δv: dictKey "ab" neg
        , fwd_expect: "{[\"a\"] : 5, [⸨\"ab\"⸩] : 6}"
        , datasets: []
        }
      , { file: "dict/difference"
        , imports: []
        , bwd_expect_file: "dict/difference.expect"
        , δv: dict neg
        , fwd_expect: "⸨{[\"a\"] : 5}⸩"
        , datasets: []
        }
      , { file: "dict/disjointUnion"
        , imports: []
        , bwd_expect_file: "dict/disjointUnion.expect"
        , δv: dictKey "a" neg >>> dictVal "c" neg
        , fwd_expect: "{[⸨\"a\"⸩] : 5, [\"b\"] : 6, [\"c\"] : ⸨7⸩}"
        , datasets: []
        }
      , { file: "dict/foldl", imports: [], bwd_expect_file: "dict/foldl.expect", δv: neg, fwd_expect: "⸨0⸩", datasets: [] }
      , { file: "dict/intersectionWith"
        , imports: []
        , bwd_expect_file: "dict/intersectionWith.expect"
        , δv: dictVal "b" neg >>> dictVal "c" neg
        , fwd_expect: "{[\"b\"] : ⸨0⸩, [\"c\"] : ⸨20⸩}"
        , datasets: []
        }
      , { file: "dict/get", imports: [], bwd_expect_file: "dict/get.expect", δv: neg, fwd_expect: "⸨0⸩", datasets: [] }
      , { file: "dict/map", imports: [], bwd_expect_file: "dict/map.expect", δv: neg, fwd_expect: "⸨20⸩", datasets: [] }
      , { file: "dict/match", imports: [], bwd_expect_file: "dict/match.expect", δv: neg, fwd_expect: "", datasets: [] }
      , { file: "list-comp"
        , imports: []
        , bwd_expect_file: "list-comp-1.expect"
        , δv: listCell 1 neg
        , fwd_expect: "(6.2 : ⸨(260 : (19.9 : (91 : [])))⸩)"
        , datasets: []
        }
      , { file: "list-comp"
        , imports: []
        , bwd_expect_file: "list-comp-2.expect"
        , δv: listCell 2 neg
        , fwd_expect: "(6.2 : (260 : ⸨(19.9 : (91 : []))⸩))"
        , datasets: []
        }
      ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
