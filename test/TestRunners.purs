module Test.TestRunners where

import Prelude

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Data.Foldable (traverse_)
import Data.Set (Set) as S
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import EvalGraph (GraphConfig)
import Graph.GraphImpl (GraphImpl)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports)
import SExpr (Expr) as SE
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Mocha (runMocha)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, checkPretty, testWithSetup)
import Util (type (×), successful, (×))
import Val (Val, (<+>))

type Test a = SpecT Aff Unit Effect a
type TestWith i a = SpecT Aff i Effect a
type TestIn g i a = SpecT g i Effect a

run :: forall a. Test a -> Effect Unit
run = runMocha

withDefaultImports ∷ forall a. TestWith (GraphConfig (GraphImpl S.Set)) a -> Test a
withDefaultImports x = beforeAll openDefaultImports x

withDataset :: File -> TestWith (GraphConfig (GraphImpl S.Set)) Unit -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
withDataset dataset =
   beforeWith (openDatasetAs dataset "data" >=> ((\({ g, n, γα } × xv) -> pure { g, n, γα: γα <+> xv })))

testMany :: Array TestSpec → Boolean -> SpecT Aff Unit Effect Unit
testMany fxs is_bench = withDefaultImports $ traverse_ test fxs
   where
   test :: TestSpec -> SpecT Aff (GraphConfig (GraphImpl S.Set)) Effect Unit
   test { file, fwd_expect } =
      -- (((_ <$> open (folder <> File file)) <<< (×)) :: GraphConfig -> Aff (GraphConfig × Expr) ) 
      beforeWith ((_ <$> open (File file)) <<< (×)) $
         it (show file)
            ( \(gconfig × s) -> do
                 void $ internal (gconfig × s)
                 pure unit
            ) -- a -> g unit 
      where
      internal :: (GraphConfig (GraphImpl S.Set) × SE.Expr Unit) -> Aff Unit
      internal (gconfig × s) = do
         outs <- (testWithSetup file is_bench s gconfig { δv: identity, fwd_expect, bwd_expect: mempty })
         logShow outs

testBwdMany :: Array TestBwdSpec → Boolean -> Test Unit
testBwdMany fxs is_bench = withDefaultImports $ traverse_ testBwd fxs
   where
   testBwd :: TestBwdSpec -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
   testBwd { file, file_expect, δv, fwd_expect } =
      -- (((_ <$> open (folder <> File file)) <<< (×)) :: GraphConfig -> Aff (GraphConfig × Expr) )   
      beforeWith ((_ <$> open (folder <> File file)) <<< (×)) $
         it (show $ folder <> File file)
            \(gconfig × s) -> do
               bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
               void $ testWithSetup file is_bench s gconfig { δv, fwd_expect, bwd_expect }
   folder = File "slicing/"

testWithDatasetMany :: Array TestWithDatasetSpec -> Boolean -> Test Unit
testWithDatasetMany fxs is_bench = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset :: TestWithDatasetSpec -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
   testWithDataset { dataset, file } =
      withDataset (File dataset) $ beforeWith ((_ <$> open (File file)) <<< (×)) do
         it (show file)
            \(gconfig × s) ->
               void $ testWithSetup file is_bench s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty }

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