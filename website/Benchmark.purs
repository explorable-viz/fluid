module Website.Benchmark where

import Prelude

import Data.Array (concat)
import Data.Array.NonEmpty (fromArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Module.Web (loadFile)
import Test.Benchmark.Util (BenchAcc(..), BenchRow)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util.Suite (BenchSuite, bwdSuite, suite, withDatasetSuite)
import Util (definitely, (×), type (×))

-- Runs as webpage; would be nicer to use Mocha but that doesn't currently support returning values from tests.

main :: Effect Unit
main = launchAff_ do
   -- Mocha.run doesn't allow values to be returned from tests, so must run via HTML entrypoint
   outs <- sequence $ reportBench <$> (concat (benchmarks <@> (10 × true)))
   logShow $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

reportBench :: String × (Aff BenchRow) -> Aff (Tuple String BenchRow)
reportBench (str × row) = do
   log $ "Benchmarking " <> str
   (str × _) <$> row

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]
