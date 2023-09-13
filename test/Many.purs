module Test.Many where

import Prelude

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Benchmark.Util (BenchRow)
import Data.Array (zip)
import Data.Foldable (traverse_)
import Data.Set (Set) as S
import Effect (Effect)
import Effect.Aff (Aff)
import EvalGraph (GraphConfig)
import Graph.GraphImpl (GraphImpl)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports)
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Mocha2 (runMocha)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, checkPretty, testWithSetup)
import Util (type (×), successful, (×))
import Val (Val, (<+>))

type Test a = SpecT Aff Unit Effect a
type TestWith i a = SpecT Aff i Effect a
type TestIn g i a = SpecT g i Effect a

run :: forall a. Array (String × Aff a) -> Effect Unit
run = runMocha

withDefaultImports ∷ forall a. TestWith (GraphConfig (GraphImpl S.Set)) a -> Test a
withDefaultImports x = beforeAll openDefaultImports x

withDataset :: File -> TestWith (GraphConfig (GraphImpl S.Set)) Unit -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
withDataset dataset =
   beforeWith (openDatasetAs dataset "data" >=> ((\({ g, n, γα } × xv) -> pure { g, n, γα: γα <+> xv })))

many :: Boolean -> Array TestSpec -> Array (String × Aff BenchRow)
many is_bench fxs = zip names affs
   where
   affs = map
      ( \{ file, fwd_expect } -> do
           default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
           expr <- open (File file)
           testWithSetup file is_bench expr default { δv: identity, fwd_expect, bwd_expect: mempty }
      )
      fxs
   names = map _.file fxs

bwdMany :: Boolean -> Array TestBwdSpec -> Array (String × Aff BenchRow)
bwdMany is_bench fxs = zip names affs
   where
   folder = File "slicing/"
   affs = map
      ( \{ file, file_expect, δv, fwd_expect } -> do
           default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
           bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
           expr <- open (folder <> File file)
           testWithSetup file is_bench expr default { δv, fwd_expect, bwd_expect }
      )
      fxs
   names = map _.file fxs

withDatasetMany :: Boolean -> Array TestWithDatasetSpec -> Array (String × Aff BenchRow)
withDatasetMany is_bench fxs = zip names affs
   where
   affs = map
      ( \{ dataset, file } -> do
           default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
           { g, n, γα } × xv <- openDatasetAs (File dataset) "data" default
           let loadedData = { g, n, γα: γα <+> xv }
           expr <- open (File file)
           testWithSetup file is_bench expr loadedData { δv: identity, fwd_expect: mempty, bwd_expect: mempty }
      )
      fxs
   names = map (\spec -> spec.file) fxs

testLinkMany :: Array (LinkFigSpec × Selector Val × String) -> Test Unit
testLinkMany fxs = traverse_ testLink fxs
   where
   testLink :: (LinkFigSpec × Selector Val × String) -> Test Unit
   testLink (spec@{ x } × δv1 × v2_expect) =
      before (loadLinkFig spec) $
         it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
            \{ γ0, γ, e1, e2, t1, t2, v1 } ->
               let
                  { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
               in
                  checkPretty "Linked output" v2_expect v2'