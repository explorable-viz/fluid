module Test.Many where

import Prelude

import App.Fig (linkResult, loadLinkFig)
import Benchmark.Util (BenchRow)
import Data.Array (zip)
import Effect.Aff (Aff)
import Module (File(..), Folder(..), defaultImports2, loadFile, openDatasetAs, openDefaultImports)
import Test.Util (TestBwdSpec, TestLinkSpec, TestSpec, TestWithDatasetSpec, checkPretty, testWithSetup, testWithSetup2)
import Util (type (×), (×))
import Val (ProgCxtEval(..), (<+>))

many :: Array TestSpec -> Int -> Array (String × Aff BenchRow)
many specs n = zip (specs <#> _.file) (specs <#> one)
   where
   one { file, fwd_expect } = do
      progCxt <- defaultImports2
      testWithSetup2 n (File file) progCxt { δv: identity, fwd_expect, bwd_expect: mempty }

bwdMany :: Array TestBwdSpec -> Int -> Array (String × Aff BenchRow)
bwdMany specs n = zip (specs <#> _.file) (specs <#> bwdOne)
   where
   folder = File "slicing/"
   bwdOne { file, file_expect, δv, fwd_expect } = do
      progCxt <- defaultImports2
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
      testWithSetup2 n (folder <> File file) progCxt { δv, fwd_expect, bwd_expect }

withDatasetMany :: Array TestWithDatasetSpec -> Int -> Array (String × Aff BenchRow)
withDatasetMany specs n = zip (specs <#> _.file) (specs <#> withDatasetOne)
   where
   withDatasetOne { dataset, file } = do
      -- TODO: make progCxt consistent with addition of xv
      gconfig@{ progCxt: ProgCxtEval r@{ γ } } × xv <- openDefaultImports >>= openDatasetAs (File dataset) "data"
      testWithSetup n (File file) gconfig { progCxt = ProgCxtEval r { γ = γ <+> xv } }
         { δv: identity, fwd_expect: mempty, bwd_expect: mempty }

linkMany :: Array TestLinkSpec -> Array (String × Aff Unit)
linkMany specs = zip (specs <#> name) (specs <#> linkOne)
   where
   name spec = "linking/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2
   linkOne { spec, δv1, v2_expect } = do
      { γ, e1, e2, t1, t2, v1 } <- loadLinkFig spec
      { v': v2' } <- linkResult spec.x γ e1 e2 t1 t2 (δv1 v1)
      checkPretty "Linked output" v2_expect v2'
