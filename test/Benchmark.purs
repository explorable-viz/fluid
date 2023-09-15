module Test.Benchmark
   ( --bench_desugaring
     main
   ) where

import Prelude hiding (add)

import Benchmark.Util (BenchAcc(..), BenchRow)
import Control.Apply (lift2)
import Data.Array (concat)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Test.Many (many, bwdMany, withDatasetMany)
import Test.Spec.Specs (misc_cases, bwd_cases, desugar_cases, graphics_cases)
import Util (type (×), (×))

main :: Effect Unit
main = launchAff_ do
   let arr = concat [ bench_misc, bench_desugaring, bench_bwd, bench_graphics ]
   outs <- sequence $ map (\(str × row) -> lift2 Tuple (pure str) row) arr
   logShow $ BenchAcc outs

bench_desugaring :: Array (String × Aff BenchRow)
bench_desugaring = many desugar_cases

bench_misc :: Array (String × Aff BenchRow)
bench_misc = many misc_cases

bench_bwd :: Array (String × Aff BenchRow)
bench_bwd = bwdMany bwd_cases

bench_graphics :: Array (String × Aff BenchRow)
bench_graphics = withDatasetMany graphics_cases
