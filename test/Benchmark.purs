module Test.Benchmark
   ( bench_desugaring
   , main
   ) where

import Prelude hiding (add)

import Benchmark.BenchRunners (benchBwdMany, benchMany, benchWithDatasetMany)
import Benchmark.Util (BenchAcc(..), BenchRow)
import Control.Apply (lift2)
import Data.Array (concat)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Test.Spec.Specs (bwd_cases, desugar_cases, graphics_cases, misc_cases)
import Util (type (×), (×))

main :: Effect Unit
main = launchAff_ do
   --log "Running benchmarks!"
   let arr = concat [bench_desugaring, bench_misc,  bench_bwd, bench_graphics] 
   outs <- sequence $ map affify arr 
   logShow $ BenchAcc outs

bench_desugaring :: Array (String × Aff BenchRow)
bench_desugaring = benchMany desugar_cases

bench_misc :: Array (String × Aff BenchRow)
bench_misc = benchMany misc_cases

bench_bwd :: Array (String × Aff BenchRow)
bench_bwd = benchBwdMany bwd_cases

bench_graphics :: Array (String × Aff BenchRow)
bench_graphics = benchWithDatasetMany graphics_cases

affify :: String × Aff BenchRow -> Aff (String × BenchRow)
affify (str × row) = lift2 Tuple (pure str) row 