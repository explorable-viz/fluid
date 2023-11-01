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
   let
      iter = 3
      arr = concat ([ bench_misc, bench_desugaring, bench_bwd, bench_graphics ] <#> ((#) (iter × true)))
   outs <- sequence $ (\(str × row) -> (str × _) <$> row) <$> arr
   logShow $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

bench_desugaring :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_desugaring = many desugar_cases

bench_misc :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_misc = many misc_cases

bench_bwd :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_bwd = bwdMany bwd_cases

bench_graphics :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_graphics = withDatasetMany graphics_cases
