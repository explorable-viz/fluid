module Test.Benchmark where

import Prelude
import Data.Array (concat)
import Data.Array.NonEmpty (fromArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Test.Benchmark.Util (BenchAcc(..))
import Test.Specs (misc_cases, bwd_cases, desugar_cases, graphics_cases)
import Test.Util.Suite (BenchSuite, bwdSuite, suite, withDatasetSuite)
import Util (definitely, (×))

main :: Effect Unit
main = launchAff_ do
   outs <- sequence $ (\(str × row) -> (str × _) <$> row) <$> (concat (benchmarks <#> (_ $ (3 × true))))
   logShow $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

benchmarks :: Array BenchSuite
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
