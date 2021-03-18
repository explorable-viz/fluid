module Test.Main where

import Prelude
import Data.Array (concat)
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import DataType (cPair)
import Test.Util (Test, run, test, testWithDataset, test_bwd)
import Val (Val(..))

tests :: Array (Array (Test Unit))
tests = [ test_desugaring, test_misc, test_slicing, test_graphics ]
--tests = [ test_scratchpad ]

main :: Effect Unit
main = void (sequence (run <$> concat tests))

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
--   test "scratchpad" "17"
   test_bwd "slicing/array-dims" (Constr false cPair (Int true 3 : Int true 3 : Nil)) "(_3_, _3_)"
]

test_slicing :: Array (Test Unit)
test_slicing = [
   test_bwd "slicing/add" (Int true 8) "_8_",
   test_bwd "slicing/divide" Hole "0.75",
   test_bwd "slicing/array-lookup" (Int true 17) "_17_",
   test_bwd "slicing/multiply" (Int true 0) "_0_",
   test_bwd "slicing/nth" (Int true 4) "_4_"
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
