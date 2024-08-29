module Test.Util.Suite where

import Prelude

import App.Fig (selectionResult, loadFig, selectInput, selectOutput)
import App.Util (Selector, cheatToSel, compress, kindOfBotS, toR𝔹)
import App.View.Util (Fig, FigSpec)
import Bind (Bind, (↦))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (fst, snd, uncurry)
import Debug (spy)
import Effect.Aff (Aff)
import Module (File(..), Folder(..), loadFile, loadProgCxt)
import Test.Benchmark.Util (BenchRow, logTimeWhen)
import Test.Util (checkEq, test)
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
bwdSuite specs (n × is_bench) = specs <#> ((_.file >>> (unwrap folder <> _)) &&& asTest)
   where
   folder = File "slicing/"

   asTest :: TestBwdSpec -> Aff BenchRow
   asTest { imports, file, bwd_expect_file, δv, fwd_expect, datasets } = do
      gconfig <- loadProgCxt imports datasets
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
   v <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (fst (selectionResult fig))
   --there's no reason that we should be able to apply checkEq to ReactState
   --checkEq "selectedA" "expectedA" (Inert) (Reactive (SelState { persistent: true, transient: true }))
   --checkEq "selectedA" "expectedA" [(Inert), (Reactive (SelState{persistent: true, transient: true}))] [(Reactive (SelState{persistent: true, transient: true})), (Inert)]
   checkEq "selected0" "expected0" (spy "a2" <$> (compress <<< toR𝔹 <$> v)) (spy "a1" <<< toR𝔹 <$> v)
   --checkEq "selected1" "expected1" (spy "a1" <<< toR𝔹 <$> v) (spy "a2" <$> (toR𝔹 <$> v))
   --checkEq "jointest" "jointest2" (cheatToSel  ((Reactive (SelState { persistent: true, transient: false })) ∨ (Reactive (SelState { persistent: false, transient: false })))) (SelState {persistent: true, transient: false})
   --checkEq "selected2" "expected2" (spy "b1" <<< compress <<< toR𝔹 <$> v) (spy "b2" <<< compress <<< toR𝔹 <$> v)
   --checkEq "selected3" "expected3" (spy "3" <<< compress <<< toR𝔹 <$> v) ( {-spy "product" <<< -} compress <$> (out_expect (toR𝔹 <$> (kindOfBotS <$> v)))) {-(Reactive (SelState ({persistent: true, transient: true}))) (Reactive (SelState ({persistent:true, transient: true})))-} {-(spy "v" <<< toR𝔹 <$> v) (spy "product" <$> (out_expect (toR𝔹 <$> (kindOfBotS <$> v))))-}
   --checkEq "selected4" "expected4" ( {-spy "v" <<< -} nullify <<< toR𝔹 <$> v)  ( {-spy "product" <<< -} compress <$> (out_expect (toR𝔹 <$> (kindOfBotS <$> v))))) {-(Reactive (SelState ({persistent: true, transient: true})) (Reactive (SelState ({persistent:true, transient: true})))-} {-(spy "v" <<< toR𝔹 <$> v) (spy "product" <$> (out_expect (toR𝔹 <$> (kindOfBotS <$> v))))-}
   checkEq "selected" "expected" (cheatToSel <<< toR𝔹 <$> v) (cheatToSel <$> (out_expect (toR𝔹 <$> (kindOfBotS <$> v))))
   pure fig

linkedOutputsSuite :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsSuite specs = specs <#> (name &&& (linkedOutputsTest >>> void))
   where
   name { spec } = unwrap spec.file

linkedInputsTest :: TestLinkedInputsSpec -> Aff Fig
linkedInputsTest { spec, δ_in, in_expect } = do
   fig <- loadFig (spec { file = spec.file }) <#> uncurry selectInput δ_in
   γ <- logTimeWhen timing.selectionResult (unwrap spec.file) \_ ->
      pure (snd (selectionResult fig))
   checkEq "selected" "expected" (cheatToSel <<< toR𝔹 <$> γ) (cheatToSel <$> (in_expect (toR𝔹 <$> (kindOfBotS <$> γ))))
   pure fig

linkedInputsSuite :: Array TestLinkedInputsSpec -> Array (String × Aff Unit)
linkedInputsSuite specs = specs <#> (name &&& (linkedInputsTest >>> void))
   where
   name { spec } = unwrap spec.file
