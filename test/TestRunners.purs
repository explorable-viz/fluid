module Test.TestRunners where

import Prelude

import App.Fig (linkResult, loadLinkFig)
import Data.Array (zip)
import Data.Profunctor.Strong (second)
import Effect.Aff (Aff)
import Test.Many (bwdMany, many, withDatasetMany)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, TestLinkSpec, checkPretty)
import Util (type (×), successful)

testMany :: Array TestSpec -> Array (String × Aff Unit)
testMany fxs = map (second void) $ many false fxs

testBwdMany :: Array TestBwdSpec -> Array (String × Aff Unit)
testBwdMany fxs = map (second void) $ bwdMany false fxs

testWithDatasetMany :: Array TestWithDatasetSpec -> Array (String × Aff Unit)
testWithDatasetMany fxs = map (second void) $ withDatasetMany false fxs

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
