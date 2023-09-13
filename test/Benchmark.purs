module Test.Benchmark
   ( bench_desugaring
   , main
   ) where

import Prelude hiding (add)

import Benchmark.Runners (benchBwdMany, benchMany, benchWithDatasetMany)
import Benchmark.Util (BenchAcc)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Test.Spec.Specs (bwd_cases, desugar_cases, graphics_cases, misc_cases)

main :: Effect Unit
main = launchAff_ do
   log "Running benchmarks!"
   arr <- (bench_desugaring <> bench_misc <> bench_bwd <> bench_graphics)
   logShow arr

bench_desugaring :: Aff BenchAcc
bench_desugaring = benchMany desugar_cases

bench_misc :: Aff BenchAcc
bench_misc = benchMany misc_cases

bench_bwd :: Aff BenchAcc
bench_bwd = benchBwdMany bwd_cases

bench_graphics :: Aff BenchAcc
bench_graphics = benchWithDatasetMany graphics_cases