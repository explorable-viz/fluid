module Test.TestRunners where

import Prelude

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Data.Foldable (traverse_)
import Data.Profunctor.Strong (second)
import Data.Set (Set) as S
import Effect (Effect)
import Effect.Aff (Aff)
import EvalGraph (GraphConfig)
import Graph.GraphImpl (GraphImpl)
import Module (File, openDatasetAs, openDefaultImports)
import Test.Many (bwdMany, many, withDatasetMany)
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Mocha2 (runMocha)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, checkPretty)
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

testMany :: Array TestSpec -> Array (String × Aff Unit)
testMany fxs = map (second void) $ many false fxs

testBwdMany :: Array TestBwdSpec -> Array (String × Aff Unit)
testBwdMany fxs = map (second void) $ bwdMany false fxs

testWithDatasetMany :: Array TestWithDatasetSpec -> Array (String × Aff Unit)
testWithDatasetMany fxs = map (second void) $ withDatasetMany false fxs

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