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
tests = [ test_desugaring, test_misc, test_slicing, test_linking, test_graphics ]
--tests = [ test_slicing ]

main :: Effect Unit
main = void (sequence (run <$> concat tests))

pair :: 𝔹 -> Val 𝔹 -> Val 𝔹 -> Val 𝔹
pair α v1 v2 = Constr α cPair (v1 : v2 : Nil)

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
]

test_linking :: Array (Test Unit)
test_linking = [
   testLink "pairs" (pair false hole (pair false hole (pair false (Int true 3) hole))) "(3, (_5_, _7_))",
   testLink "convolution"
            (Matrix true (insertMatrix 2 2 (Hole true) (holeMatrix 5 5)))
            "_14.333333333333334_, _11.222222222222221_, _8.0_, 7.888888888888889, 15.333333333333334,\n\
            \_20.77777777777778_, _13.11111111111111_, _19.444444444444443_, 9.88888888888889, 19.22222222222222,\n\
            \_18.444444444444443_, _12.0_, _24.555555555555557_, 10.0, 12.222222222222221,\n\
            \11.555555555555555, 15.222222222222221, 16.77777777777778, 10.333333333333334, 17.333333333333332,\n\
            \4.555555555555555, 14.11111111111111, 13.333333333333334, 5.444444444444445, 17.555555555555557"
]

test_slicing :: Array (Test Unit)
test_slicing = [
   testBwd "add" (Int true 8) "_8_",
   testBwd "array-lookup" (Int true 14) "_14_",
   testBwd "array-dims" (pair true (Int true 3) (Int true 3)) "(_3_, _3_)",
   testBwd "conv-extend"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
           "_40.22222222222222_, 32.111111111111114, 22.88888888888889, 30.444444444444443, 37.888888888888886,\n\
           \36.22222222222222, 30.333333333333332, 30.22222222222222, 29.666666666666668, 33.111111111111114,\n\
           \28.555555555555557, 28.0, 34.666666666666664, 20.11111111111111, 20.333333333333332,\n\
           \21.77777777777778, 25.555555555555557, 28.88888888888889, 28.333333333333332, 27.333333333333332,\n\
           \13.666666666666666, 24.77777777777778, 27.22222222222222, 22.11111111111111, 29.444444444444443",
   testBwd "conv-wrap"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
           "_32.333333333333336_, 30.333333333333332, 25.88888888888889, 27.0, 29.22222222222222,\n\
           \30.77777777777778, 30.333333333333332, 30.22222222222222, 29.666666666666668, 32.55555555555556,\n\
           \34.0, 28.0, 34.666666666666664, 20.11111111111111, 24.444444444444443,\n\
           \23.22222222222222, 25.555555555555557, 28.88888888888889, 28.333333333333332, 25.22222222222222,\n\
           \26.11111111111111, 30.555555555555557, 26.11111111111111, 19.666666666666668, 28.77777777777778",
   testBwd "conv-zero"
           (Matrix true (insertMatrix 1 1 (Hole true) (holeMatrix 5 5)))
           "_18.666666666666668_, 20.22222222222222, 14.666666666666666, 20.555555555555557, 18.11111111111111,\n\
           \25.333333333333332, 30.333333333333332, 30.22222222222222, 29.666666666666668, 25.22222222222222,\n\
           \22.555555555555557, 28.0, 34.666666666666664, 20.11111111111111, 17.11111111111111,\n\
           \16.88888888888889, 25.555555555555557, 28.88888888888889, 28.333333333333332, 20.77777777777778,\n\
           \10.444444444444445, 21.0, 19.0, 15.0, 19.333333333333332",
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
