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
import Test.Many (bwdMany)
import Test.Spec.Specs (bwd_cases)
import Util (type (×), (×))

main :: Effect Unit
main = launchAff_ do
   let
      iter = 1
      arr = concat ([ bench_bwd ] <#> (#) iter)
   outs <- sequence $ map (\(str × row) -> lift2 Tuple (pure str) row) arr
   logShow $ BenchAcc outs

-- bench_desugaring :: Int -> Array (String × Aff BenchRow)
-- bench_desugaring = many desugar_cases

-- bench_misc :: Int -> Array (String × Aff BenchRow)
-- bench_misc = many misc_cases

bench_bwd :: Int -> Array (String × Aff BenchRow)
bench_bwd = bwdMany bwd_cases

-- bench_graphics :: Int -> Array (String × Aff BenchRow)
-- bench_graphics = withDatasetMany graphics_cases
