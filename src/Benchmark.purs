module Benchmark where

import Prelude

import Data.Array (concat)
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class.Console (log)
import Module.Node (loadFile)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Test.Benchmark.Util (BenchAcc(..))
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util.Suite (BenchSuite, bwdSuite, suite, withDatasetSuite)
import Util (definitely, error, (×), type (+))

main :: Effect Unit
main = runAff_ cb do
   outs <- sequence $
      ( \(str × row) -> do
           log $ "Benching: " <> str
           (str × _) <$> row
      )
         <$> (concat (benchmarks <@> (10 × true)))
   pure $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

cb :: Error + BenchAcc -> Effect Unit
cb (Left err) = error $ show err
cb (Right bacc) = writeTextFile ASCII "benchmark/benchmarks_artifact.csv" $ show bacc

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]