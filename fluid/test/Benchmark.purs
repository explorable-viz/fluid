module Benchmark where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Array (concat)
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..))
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import File (class LoadFile, FileCxt(..))
import Module.Node (runNodeT)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Test.Benchmark.Util (BenchAcc(..))
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (fluidSrcPaths)
import Test.Util.Suite (BenchSuite, bwdSuite, suite)
import Util (definitely, error, (×), type (+))

main :: Effect Unit
main = runAff_ handleBench do
   outs <- sequence $
      ( \(str × row) -> do
           log $ "Benchmarking: " <> str
           (str × _) <$> row
      )
         <$> second (runNodeT (FileCxt { fluidSrcPaths }))
         <$> concat (benchmarks <@> (10 × true))
   pure $ BenchAcc $ definitely "More than one benchmark" $ fromArray outs

handleBench :: Error + BenchAcc -> Effect Unit
handleBench (Left err) = error $ show err
handleBench (Right bacc) = do
   writeTextFile ASCII "benchmark/benchmarks_artifact.csv" $ show bacc
   log "Benchmarking data written to benchmark/benchmarks_artifact.csv"

benchmarks :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (BenchSuite m)
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , bwdSuite bwd_cases
   , suite graphics_cases
   ]
