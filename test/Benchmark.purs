module Test.Benchmark
   ( --bench_desugaring
     main
   ) where

import Prelude hiding (add)

import Benchmark.Util (BenchAcc(..), BenchRow)
import Data.Array (concat)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Test.Many (many, bwdMany, withDatasetMany)
import Test.Spec.Specs (misc_cases, bwd_cases, desugar_cases, graphics_cases)
import Util (type (×), (×))

main :: Effect Unit
main = launchAff_ do
   let
      iter = 1
      arr = concat ([ bench_misc, bench_desugaring, bench_bwd, bench_graphics ] <#> ((#) (iter × true)))
   outs <- sequence $ (\(str × row) -> (str × _) <$> row) <$> arr
   logShow (BenchAcc outs)

bench_desugaring :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_desugaring = many desugar_cases

bench_misc :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_misc = many misc_cases

bench_bwd :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_bwd = bwdMany bwd_cases

bench_graphics :: (Int × Boolean) -> Array (String × Aff BenchRow)
bench_graphics = withDatasetMany graphics_cases
