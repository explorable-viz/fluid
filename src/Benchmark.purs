module Benchmark where

import Prelude

import Benchotron.Core (Benchmark, benchFn', mkBenchmark)
import Control.Monad.Gen.Common (genTuple)
import Data.Foldable (foldl)
import Data.Set (Set, fromFoldable)
import Data.String.Gen (genDigitString)
import Data.Tuple (uncurry, Tuple(..))
import Effect (Effect)
import Effect.Console (logShow, log)
import Graph (Vertex(..), fwdSlice, emptyG, starInOut, union, inE)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

main :: Effect Unit
main = graphTestScript

--runSuite [ benchOutStar ]

graphTestScript :: Effect Unit
graphTestScript = do
   let
      ids = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      graph = foldl (\g α -> union (Vertex (show α)) (fromFoldable $ map (Vertex <<< show) [ α + 2, α + 3 ]) g) emptyG ids
   let
      slice = fwdSlice (fromFoldable [ (Vertex "13"), (Vertex "12"), Vertex "11" ]) graph
   log ("Outedges: " <> show (inE (fromFoldable [ (Vertex "11") ]) graph))
   --logShow graph
   logShow slice

preProcessTuple :: Tuple Vertex (Array Vertex) -> Tuple Vertex (Set Vertex)
preProcessTuple (Tuple α αs) = Tuple α (fromFoldable αs)

benchOutStar :: Benchmark
benchOutStar = mkBenchmark
   { slug: "out-star"
   , title: "comparing fold and fromFoldable"
   , sizes: [ 10, 20, 40, 50, 100 ]
   , sizeInterpretation: "Size of graph being created"
   , inputsPerSize: 3
   , gen: \n -> genTuple (Vertex <$> genDigitString) (vectorOf n (Vertex <$> arbitrary))
   , functions:
        [ benchFn' "outStarFromFoldable" (uncurry starInOut) preProcessTuple
        ]
   }
