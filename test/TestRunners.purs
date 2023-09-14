module Test.TestRunners where

import Prelude

import App.Fig (linkResult, loadLinkFig)
import Data.Array (zip)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Many (bwdMany, many, withDatasetMany)
import Test.Spec.Mocha (runMocha)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, TestLinkSpec, checkPretty)
import Util (type (×), successful)

run :: forall a. Array (String × Aff a) -> Effect Unit
run = runMocha

-- withDefaultImports ∷ forall a. TestWith (GraphConfig GraphImpl) a -> Test a
-- withDefaultImports x = beforeAll openDefaultImports x

-- withDataset :: File -> TestWith (GraphConfig (GraphImpl)) Unit -> TestWith (GraphConfig (GraphImpl)) Unit
-- withDataset dataset =
--    beforeWith
--       ( openDatasetAs dataset "data" >=>
--            \({ g, n, γα } × xv) -> pure { g, n, γα: γα <+> xv }
--       )

testMany :: Array TestSpec -> Array (String × Aff Unit)
testMany fxs = map (second void) $ many false fxs

testBwdMany :: Array TestBwdSpec -> Array (String × Aff Unit)
testBwdMany fxs = map (second void) $ bwdMany false fxs

testWithDatasetMany :: Array TestWithDatasetSpec -> Array (String × Aff Unit)
testWithDatasetMany fxs = map (second void) $ withDatasetMany false fxs

-- testLinkMany :: Array (LinkFigSpec × Selector Val × String) -> Test Unit
-- testLinkMany fxs = traverse_ testLink fxs
--    where
--    testLink :: (LinkFigSpec × Selector Val × String) -> Test Unit
--    testLink (spec@{ x } × δv1 × v2_expect) =
--       before (loadLinkFig spec) $
--          it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
--             \{ γ0, γ, e1, e2, t1, t2, v1 } ->
--                let
--                   { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
--                in
--                   checkPretty "Linked output" v2_expect v2'

linkMany :: Array TestLinkSpec -> Array (String × Aff Unit)
linkMany fxs = zip names affs
   where
   names = map (\spec -> "linking/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2) fxs
   affs = map
      ( \{ spec, δv1, v2_expect } -> do
           { γ0, γ, e1, e2, t1, t2, v1 } <- loadLinkFig spec
           let { v': v2' } = successful $ linkResult spec.x γ0 γ e1 e2 t1 t2 (δv1 v1)
           checkPretty "Linked output" v2_expect v2'
      )
      fxs
