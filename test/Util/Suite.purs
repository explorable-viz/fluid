module Test.Util.Suite
   ( BenchSuite
   , TestBwdSpec
   , TestLinkedInputsSpec
   , TestLinkedOutputsSpec
   , TestSpec
   , TestWithDatasetSpec
   , bwdSuite
   , linkedInputsSuite
   , linkedOutputsSuite
   , linkedOutputsTest
   , suite
   , withDatasetSuite
   ) where

import Prelude

import App.Fig (loadFig, selectInput, selectOutput, selectionResult)
import App.Util (Selector, isInert, isPersistent, isTransient, selStates)
import App.View.Util (Fig, FigSpec)
import Bind (Bind, (↦))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Lattice (botOf)
import Module ((</>), File(..), Folder(..), FileLoader, loadProgCxt)
import Test.Benchmark.Util (BenchRow, logTimeWhen)
import Test.Util (checkEq, fluidSrcPaths, test)
import Test.Util.Debug (timing)
import Util (type (×), (×))
import Val (Val, Env)

-- benchmarks parameterised on number of iterations
type BenchSuite = Int × Boolean -> Array (String × Aff BenchRow)

type TestSpec =
   { imports :: Array String
   , file :: String
   , fwd_expect :: String
   }

type TestBwdSpec =
   { imports :: Array String
   , file :: String
   , bwd_expect_file :: String
   , δv :: Selector Val -- relative to bot
   , fwd_expect :: String
   , datasets :: Array (Bind String)
   }

type TestWithDatasetSpec =
   { dataset :: Bind String
   , imports :: Array String
   , file :: String
   }

type TestLinkedOutputsSpec =
   { spec :: FigSpec
   , δ_out :: Selector Val
   , out_expect :: Selector Val
   }

type TestLinkedInputsSpec =
   { spec :: FigSpec
   , δ_in :: Bind (Selector Val)
   , in_expect :: Selector Env
   }

suite :: FileLoader Aff -> Array TestSpec -> BenchSuite
suite loadFile specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestSpec -> Aff BenchRow
   asTest { imports, file, fwd_expect } = do
      gconfig <- loadProgCxt { loadFile, fluidSrcPaths } imports []
      test loadFile (File file) gconfig { δv: identity, fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdSuite :: FileLoader Aff -> Array TestBwdSpec -> BenchSuite
bwdSuite loadFile specs (n × is_bench) = specs <#> ((_.file >>> File >>> (folder </> _) >>> show) &&& asTest)
   where
   folder = Folder "slicing"

   asTest :: TestBwdSpec -> Aff BenchRow
   asTest { imports, file, bwd_expect_file, δv, fwd_expect, datasets } = do
      gconfig <- loadProgCxt { loadFile, fluidSrcPaths } imports datasets
      bwd_expect <- loadFile [ Folder "test/fluid" ] (folder </> File bwd_expect_file)
      test loadFile (folder </> File file) gconfig { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetSuite :: FileLoader Aff -> Array TestWithDatasetSpec -> BenchSuite
withDatasetSuite loadFile specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestWithDatasetSpec -> Aff BenchRow
   asTest { imports, dataset: x ↦ dataset, file } = do
      gconfig <- loadProgCxt { loadFile, fluidSrcPaths } imports [ x ↦ dataset ]
      test loadFile (File file) gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsTest :: TestLinkedOutputsSpec -> Aff Fig
linkedOutputsTest { spec, δ_out, out_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> selectOutput δ_out
   v <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (selectionResult fig).v
   checkEq "selected" "expected" (selStates <$> (isInert <$> v) <*> (isPersistent <$> v) <*> (isTransient <$> v)) (out_expect (botOf <$> v))
   pure fig

linkedOutputsSuite :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsSuite specs = specs <#> (name &&& (linkedOutputsTest >>> void))
   where
   name { spec } = unwrap spec.file

linkedInputsTest :: TestLinkedInputsSpec -> Aff Fig
linkedInputsTest { spec, δ_in, in_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> uncurry selectInput δ_in
   γ <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (selectionResult fig).γ
   checkEq "selected" "expected" (selStates <$> (isInert <$> γ) <*> (isPersistent <$> γ) <*> (isTransient <$> γ)) (in_expect (botOf <$> γ))
   pure fig

linkedInputsSuite :: Array TestLinkedInputsSpec -> Array (String × Aff Unit)
linkedInputsSuite specs = specs <#> (name &&& (linkedInputsTest >>> void))
   where
   name { spec } = unwrap spec.file
