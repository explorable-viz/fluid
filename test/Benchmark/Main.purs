module Test.Benchmark.Main where

import Prelude
import Data.Array (concat)
import Data.Array.NonEmpty (fromArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Test.Benchmark.Util (BenchAcc(..), BenchRow)
import Test.Specs (misc_cases, bwd_cases, desugar_cases, graphics_cases)
import Test.Util.Many (many, bwdMany, withDatasetMany)
import Util (type (×), definitely, (×))

main :: Effect Unit
main = launchAff_ do
   outs <- sequence $ (\(str × row) -> (str × _) <$> row) <$> (concat (benchmarks <#> (_ $ (3 × true))))
   logShow $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

-- benchmarks parameterised on number of iterations
type BenchSuite = (Int × Boolean) -> Array (String × Aff BenchRow)

benchmarks :: Array BenchSuite
benchmarks =
   [ many desugar_cases
   , many misc_cases
   , bwdMany bwd_cases
   , withDatasetMany graphics_cases
   ]
