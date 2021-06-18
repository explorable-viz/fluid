module Test.Main where

import Prelude
import Data.Array (concat)
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import DataType (cCons)
import Lattice (𝔹)
import Module (File(..))
import Test.Util (Test, run, selectCell, selectPair, test, testBwd, testLink, testWithDataset)
import Val (Val(..))

tests :: Array (Array (Test Unit))
--tests = [ test_desugaring, test_misc, test_bwd, test_linking, test_graphics ]
tests = [ test_scratchpad ]

main :: Effect Unit
main = void (sequence (run <$> concat tests))

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
   testLink (File "record-view-1") (File "record-view-2") (File "record-view-data")
            hole
            "[{x: 2015, y: 16.7}]"
]

test_linking :: Array (Test Unit)
test_linking = [
   testLink (File "pairs-1") (File "pairs-2") (File "pairs-data")
            (selectPair false hole (selectPair false hole (selectPair false (Int true 3) hole))) "(3, (_5_, _7_))",
   testLink (File "convolution-1") (File "convolution-2") (File "convolution-data")
            (selectCell 2 2 5 5)
            "_18_, _12_, _13_, 9, 19,\n\
            \_20_, _11_, _24_, 9, 14,\n\
            \_15_, _13_, _20_, 11, 14,\n\
            \7, 15, 15, 8, 20,\n\
            \3, 10, 12, 3, 11",
   testLink (File "bar-chart") (File "line-chart") (File "renewables")
            hole
            "LineChart ({\
               \caption: \"Growth in renewables for USA\", \
               \plots: [\
                  \LinePlot ({\
                     \name: \"Bio\", \
                     \data: [\
                        \{x: 2013, y: □}, {x: 2014, y: □}, {x: 2015, y: 16.7}, {x: 2016, y: □}, {x: 2017, y: □}, {x: 2018, y: □}\
                     \]\
                  \}), \
                  \LinePlot ({\
                     \name: \"Hydro\", \
                     \data: [\
                        \{x: 2013, y: □}, {x: 2014, y: □}, {x: 2015, y: 80}, {x: 2016, y: □}, {x: 2017, y: □}, {x: 2018, y: □}\
                     \]\
                  \}), \
                  \LinePlot ({\
                     \name: \"Solar\", \
                     \data: [\
                        \{x: 2013, y: □}, {x: 2014, y: □}, {x: 2015, y: 26}, {x: 2016, y: □}, {x: 2017, y: □}, {x: 2018, y: □}\
                     \]\
                  \}), \
                  \LinePlot ({\
                     \name: \"Wind\", \
                     \data: [\
                        \{x: 2013, y: □}, {x: 2014, y: □}, {x: 2015, y: 74}, {x: 2016, y: □}, {x: 2017, y: □}, {x: 2018, y: □}\
                     \]\
                  \})\
               \]\
            \})"
]

test_bwd :: Array (Test Unit)
test_bwd = [
   testBwd (File "add") (Int true 8) "_8_",
   testBwd (File "array-lookup") (Int true 14) "_14_",
   testBwd (File "array-dims") (selectPair true (Int true 3) (Int true 3)) "(_3_, _3_)",
   testBwd (File "conv-extend")
           (selectCell 1 1 5 5)
            "_0_, -1, 2, 0, -1,\n\
            \0, 3, -2, 3, -2,\n\
            \-1, 1, -5, 0, 4,\n\
            \1, -1, 4, 0, -4,\n\
            \1, 0, -3, 2, 0",
   testBwd (File "conv-wrap")
           (selectCell 1 1 5 5)
           "_1_, 2, -1, 1, 5,\n\
           \-1, 1, 2, -1, 1,\n\
           \0, 0, 1, 0, 1,\n\
           \0, 1, -2, 0, 1,\n\
           \0, 3, 0, 2, 2",
   testBwd (File "conv-zero")
           (selectCell 1 1 5 5)
           "_38_, 37, 28, 30, 38,\n\
           \38, 36, 46, 31, 34,\n\
           \37, 41, 54, 34, 20,\n\
           \21, 35, 31, 31, 42,\n\
           \13, 32, 35, 19, 26",
   testBwd (File "divide") (Hole true) "_40.22222222222222_",
   testBwd (File "map")
            (Constr true cCons (Hole false : (Constr true cCons (Hole false : Hole false : Nil)) : Nil)) "[5, 6]",
   testBwd (File "multiply") (Int true 0) "_0_",
   testBwd (File "nth") (Int true 4) "_4_"
]

test_desugaring :: Array (Test Unit)
test_desugaring = [
   test (File "desugar/list-comp-1") "[14, 12, 10, 13, 11, 9, 12, 10, 8]",
   test (File "desugar/list-comp-2")
        "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]",
   test (File "desugar/list-comp-3") "[9, 8]",
   test (File "desugar/list-comp-4") "[5, 4, 3]",
   test (File "desugar/list-comp-5") "[5, 4, 3]",
   test (File "desugar/list-comp-6") "[5]",
   test (File "desugar/list-comp-7") "[[]]",
   test (File "desugar/list-enum") "[3, 4, 5, 6, 7]"
]

test_misc :: Array (Test Unit)
test_misc = [
   test (File "arithmetic") "42",
   test (File "array") "(1, (3, 3))",
   test (File "compose") "5",
   test (File "div-mod-quot-rem") "[[1, -1, -2, 2], [2, 2, 1, 1], [1, -1, -1, 1], [2, 2, -2, -2]]",
   test (File "factorial") "40320",
   test (File "filter") "[8, 7]",
   test (File "flatten") "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]",
   test (File "foldr_sumSquares") "661",
   test (File "lexicalScoping") "\"6\"",
   test (File "length") "2",
   test (File "lookup") "Some \"sarah\"",
   test (File "map") "[5, 7, 13, 15, 4, 3, -3]",
   test (File "mergeSort") "[1, 2, 3]",
   test (File "normalise") "(33, 66)",
   test (File "pattern-match") "4",
   test (File "range") "[(0, 0), (0, 1), (1, 0), (1, 1)]",
   test (File "records") "{a: 5, b: 6, c: 7, d: [5], e: 7}",
   test (File "reverse") "[2, 1]",
   test (File "zipWith") "[[10], [12], [20]]"
]

test_graphics :: Array (Test Unit)
test_graphics = [
   testWithDataset (File "dataset/renewables-restricted") (File "graphics/background"),
   testWithDataset (File "dataset/renewables-restricted") (File "graphics/grouped-bar-chart"),
   testWithDataset (File "dataset/renewables-restricted") (File "graphics/line-chart"),
   testWithDataset (File "dataset/renewables-restricted") (File "graphics/stacked-bar-chart")
]
