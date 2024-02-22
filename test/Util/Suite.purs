module Test.Util.Suite where

import Prelude

import App.Fig (Fig, FigSpec, selectionResult, loadFig, selectInput, selectOutput)
import App.Util (to𝔹)
import Bind (Bind, (↦))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((&&&))
import Effect.Aff (Aff)
import Lattice (botOf)
import Module (File(..), Folder(..), loadFile, loadProgCxt)
import Test.Benchmark.Util (BenchRow, logTimeWhen)
import Test.Util (Selector, checkEq, test)
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

suite :: Array TestSpec -> BenchSuite
suite specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestSpec -> Aff BenchRow
   asTest { imports, file, fwd_expect } = do
      gconfig <- loadProgCxt imports []
      test (File file) gconfig { δv: identity, fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdSuite :: Array TestBwdSpec -> BenchSuite
bwdSuite specs (n × is_bench) = specs <#> ((_.file >>> ("slicing/" <> _)) &&& asTest)
   where
   folder = File "slicing/"

   asTest :: TestBwdSpec -> Aff BenchRow
   asTest { imports, file, bwd_expect_file, δv, fwd_expect } = do
      gconfig <- loadProgCxt imports []
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File bwd_expect_file)
      test (folder <> File file) gconfig { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetSuite :: Array TestWithDatasetSpec -> BenchSuite
withDatasetSuite specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestWithDatasetSpec -> Aff BenchRow
   asTest { imports, dataset: x ↦ dataset, file } = do
      gconfig <- loadProgCxt imports [ x ↦ dataset ]
      test (File file) gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsTest :: TestLinkedOutputsSpec -> Aff Fig
linkedOutputsTest { spec, δ_out, out_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> selectOutput δ_out
   out × _ <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ -> pure $ selectionResult fig
   checkEq "selected" "expected" (to𝔹 <$> out) (out_expect (botOf out))
   pure fig

linkedOutputsSuite :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsSuite specs = specs <#> (name &&& (linkedOutputsTest >>> void))
   where
   name { spec } = unwrap spec.file

linkedInputsTest :: TestLinkedInputsSpec -> Aff Fig
linkedInputsTest { spec, δ_in, in_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> selectInput δ_in
   _ × γ <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ -> pure $ selectionResult fig
   checkEq "selected" "expected" (to𝔹 <$> γ) (in_expect (botOf γ))
   pure fig

linkedInputsSuite :: Array TestLinkedInputsSpec -> Array (String × Aff Unit)
linkedInputsSuite specs = specs <#> (name &&& (linkedInputsTest >>> void))
   where
   name { spec } = unwrap spec.file
