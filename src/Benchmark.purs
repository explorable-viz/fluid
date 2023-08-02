module Benchmark where
  
import Prelude
import Effect (Effect)
import Benchotron.Core (Benchmark, benchFn', mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Gen.Common (genTuple)
import Data.Set (fromFoldable, Set)
import Data.String.Gen (genDigitString)
import Data.Tuple (uncurry, Tuple(..))
import Graph (outStar, outStar', Vertex(..))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)


main :: Effect Unit
main = runSuite [benchOutStar]

preProcessTuple :: Tuple Vertex (Array Vertex) -> Tuple Vertex (Set Vertex)
preProcessTuple (Tuple α αs) = Tuple α (fromFoldable αs)

benchOutStar :: Benchmark
benchOutStar = mkBenchmark 
    {   slug: "out-star"
    ,   title: "comparing fold and fromFoldable"
    ,   sizes: [10, 20, 40, 50, 100]
    ,   sizeInterpretation: "Size of graph being created"
    ,   inputsPerSize: 3
    ,   gen: \n -> genTuple (Vertex <$> genDigitString) (vectorOf n (Vertex <$> arbitrary))
    ,   functions: [ benchFn' "outStarFold" (uncurry outStar) preProcessTuple
                   , benchFn' "outStarFromFoldable" (uncurry outStar') preProcessTuple
                   ]
    }
