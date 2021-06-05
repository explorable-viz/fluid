module Test.Main where

import Prelude
import Data.Array (concat)
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import DataType (cCons, cPair)
import Lattice (𝔹)
import Test.Util (Test, run, test, testBwd, testLink, testWithDataset)
import Val (Val(..), holeMatrix, insertMatrix)

tests :: Array (Array (Test Unit))
-- tests = [ test_desugaring, test_misc, test_bwd, test_linking, test_graphics ]
tests = [ test_scratchpad ]

main :: Effect Unit
main = void (sequence (run <$> concat tests))

pair :: 𝔹 -> Val 𝔹 -> Val 𝔹 -> Val 𝔹
pair α v1 v2 = Constr α cPair (v1 : v2 : Nil)

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
   test "records" "{a: 5, b: 6, c: 7, d: [5]}"
]

test_linking :: Array (Test Unit)
test_linking = [
   testLink "pairs" (pair false hole (pair false hole (pair false (Int true 3) hole))) "(3, (_5_, _7_))",
   testLink "convolution"
            (Matrix true (insertMatrix 2 2 (Hole true) (holeMatrix 5 5)))
            "_18_, _12_, _13_, 9, 19,\n\
            \_20_, _11_, _24_, 9, 14,\n\
            \_15_, _13_, _20_, 11, 14,\n\
            \7, 15, 15, 8, 20,\n\
            \3, 10, 12, 3, 11"
]

test_bwd :: Array (Test Unit)
test_bwd = [
   testBwd "add" (Int true 8) "_8_",
   testBwd "array-lookup" (Int true 14) "_14_",
   testBwd "array-dims" (pair true (Int true 3) (Int true 3)) "(_3_, _3_)",
   testBwd "conv-extend"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
            "_0_, -1, 2, 0, -1,\n\
            \0, 3, -2, 3, -2,\n\
            \-1, 1, -5, 0, 4,\n\
            \1, -1, 4, 0, -4,\n\
            \1, 0, -3, 2, 0",
   testBwd "conv-wrap"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
           "_1_, 2, -1, 1, 5,\n\
           \-1, 1, 2, -1, 1,\n\
           \0, 0, 1, 0, 1,\n\
           \0, 1, -2, 0, 1,\n\
           \0, 3, 0, 2, 2",
   testBwd "conv-zero"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
           "_38_, 37, 28, 30, 38,\n\
           \38, 36, 46, 31, 34,\n\
           \37, 41, 54, 34, 20,\n\
           \21, 35, 31, 31, 42,\n\
           \13, 32, 35, 19, 26",
   testBwd "divide" (Hole true) "_40.22222222222222_",
   testBwd "map"
            (Constr true cCons (Hole false : (Constr true cCons (Hole false : Hole false : Nil)) : Nil)) "[5, 6]",
   testBwd "multiply" (Int true 0) "_0_",
   testBwd "nth" (Int true 4) "_4_"
]

test_desugaring :: Array (Test Unit)
test_desugaring = [
   test "desugar/list-comp-1" "[14, 12, 10, 13, 11, 9, 12, 10, 8]",
   test "desugar/list-comp-2"
        "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]",
   test "desugar/list-comp-3" "[9, 8]",
   test "desugar/list-comp-4" "[5, 4, 3]",
   test "desugar/list-comp-5" "[5, 4, 3]",
   test "desugar/list-comp-6" "[5]",
   test "desugar/list-comp-7" "[[]]",
   test "desugar/list-enum" "[3, 4, 5, 6, 7]"
]

test_misc :: Array (Test Unit)
test_misc = [
   test "arithmetic" "42",
   test "array" "(1, (3, 3))",
   test "compose" "5",
   test "div-mod-quot-rem" "[[1, -1, -2, 2], [2, 2, 1, 1], [1, -1, -1, 1], [2, 2, -2, -2]]",
   test "factorial" "40320",
   test "filter" "[8, 7]",
   test "flatten" "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]",
   test "foldr_sumSquares" "661",
   test "lexicalScoping" "\"6\"",
   test "length" "2",
   test "lookup" "Some \"sarah\"",
   test "map" "[5, 7, 13, 15, 4, 3, -3]",
   test "mergeSort" "[1, 2, 3]",
   test "normalise" "(33, 66)",
   test "pattern-match" "4",
   test "range" "[(0, 0), (0, 1), (1, 0), (1, 1)]",
   test "records" "5",
   test "reverse" "[2, 1]",
   test "zipWith" "[[10], [12], [20]]"
]

test_graphics :: Array (Test Unit)
test_graphics = [
   testWithDataset "renewables-restricted" "graphics/background",
   testWithDataset "renewables-restricted" "graphics/grouped-bar-chart",
   testWithDataset "renewables-restricted" "graphics/line-chart",
   testWithDataset "renewables-restricted" "graphics/stacked-bar-chart"
]
