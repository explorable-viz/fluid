module Test.Util.Suite where

import Prelude

import App.Fig (loadFig, selectInput, selectOutput, selectionResult)
import App.Util (SelectionType(..), Selector, isInert, isPersistent, isTransient, selStates)
import App.View.Util (Fig, FigSpec)
import Bind (Bind, (↦))
import Control.Monad.Error.Class (class MonadError)
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (fst, uncurry)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import File (class LoadFile, File(..), Folder(..), loadFile, (</>))
import Lattice (botOf)
import Module (loadProgCxt)
import Test.Benchmark.Util (BenchRow, logTimeWhen)
import Test.Util (checkEq, fluidSrcPaths, test)
import Test.Util.Debug (timing)
import Util (type (×), (×))
import Val (Val, Env)

-- benchmarks parameterised on number of iterations
type BenchSuite m = Int × Boolean -> Array (String × m BenchRow)

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

suite :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array TestSpec -> BenchSuite m
suite specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestSpec -> m BenchRow
   asTest { imports, file, fwd_expect } = do
      gconfig <- loadProgCxt { fluidSrcPaths } imports []
      test (File file) gconfig { δv: identity >>> (_ × Persistent), fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array TestBwdSpec -> BenchSuite m
bwdSuite specs (n × is_bench) = specs <#> ((_.file >>> File >>> (folder </> _) >>> show) &&& asTest)
   where
   folder = Folder "slicing"

   asTest :: TestBwdSpec -> m BenchRow
   asTest { imports, file, bwd_expect_file, δv, fwd_expect, datasets } = do
      gconfig <- loadProgCxt { fluidSrcPaths } imports datasets
      bwd_expect <- loadFile [ Folder "test/fluid" ] (folder </> File bwd_expect_file)
      test (folder </> File file) gconfig { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array TestWithDatasetSpec -> BenchSuite m
withDatasetSuite specs (n × is_bench) = specs <#> (_.file &&& asTest)
   where
   asTest :: TestWithDatasetSpec -> m BenchRow
   asTest { imports, dataset: x ↦ dataset, file } = do
      gconfig <- loadProgCxt { fluidSrcPaths } imports [ x ↦ dataset ]
      test (File file) gconfig { δv: identity >>> (_ × Persistent), fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsTest :: forall m. MonadAff m => MonadError Error m => LoadFile m => TestLinkedOutputsSpec -> m Fig
linkedOutputsTest { spec, δ_out, out_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> selectOutput δ_out
   v <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (selectionResult fig).v
   checkEq "selected" "expected" (selStates <$> (isInert <$> v) <*> (isPersistent <$> v) <*> (isTransient <$> v)) (fst $ out_expect (botOf <$> v))
   pure fig

linkedOutputsSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array TestLinkedOutputsSpec -> Array (String × m Unit)
linkedOutputsSuite specs = specs <#> (name &&& (linkedOutputsTest >>> void))
   where
   name { spec } = unwrap spec.file

linkedInputsTest :: forall m. MonadAff m => MonadError Error m => LoadFile m => TestLinkedInputsSpec -> m Fig
linkedInputsTest { spec, δ_in, in_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> uncurry selectInput δ_in
   γ <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (selectionResult fig).γ
   checkEq "selected" "expected" (selStates <$> (isInert <$> γ) <*> (isPersistent <$> γ) <*> (isTransient <$> γ)) (fst $ in_expect (botOf <$> γ))
   pure fig

linkedInputsSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array TestLinkedInputsSpec -> Array (String × m Unit)
linkedInputsSuite specs = specs <#> (name &&& (linkedInputsTest >>> void))
   where
   name { spec } = unwrap spec.file
