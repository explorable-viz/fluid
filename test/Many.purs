module Test.Many where

import Prelude

import App.Fig (linkResult, loadLinkFig)
import Benchmark.Util (BenchRow)
import Data.Array (zip)
import Data.List.Lazy (replicateM)
import Effect.Aff (Aff)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports)
import Test.Util (TestBwdSpec, TestLinkSpec, TestSpec, TestWithDatasetSpec, averageRows, checkPretty, testWithSetup)
import Util (type (×), (×), successful)
import Val (ProgCxt(..), (<+>))

many :: Array TestSpec -> Int -> Array (String × Aff BenchRow)
many specs iter = zip (specs <#> _.file) (specs <#> one)
   where
   one { file, fwd_expect } = do
      gconfig <- openDefaultImports
      expr <- open (File file)
      rows <- replicateM iter $
         testWithSetup file expr gconfig { δv: identity, fwd_expect, bwd_expect: mempty }
      pure $ averageRows rows

bwdMany :: Array TestBwdSpec -> Int -> Array (String × Aff BenchRow)
bwdMany specs iter = zip (specs <#> _.file) (specs <#> bwdOne)
   where
   folder = File "slicing/"
   bwdOne { file, file_expect, δv, fwd_expect } = do
      gconfig <- openDefaultImports
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
      expr <- open (folder <> File file)
      rows <- replicateM iter $
         testWithSetup file expr gconfig { δv, fwd_expect, bwd_expect }
      pure $ averageRows rows

withDatasetMany :: Array TestWithDatasetSpec -> Int -> Array (String × Aff BenchRow)
withDatasetMany specs iter = zip (specs <#> _.file) (specs <#> withDatasetOne)
   where
   withDatasetOne { dataset, file } = do
      -- TODO: make progCxt consistent with addition of xv
      gconfig@{ progCxt: ProgCxt r@{ γ } } × xv <- openDefaultImports >>= openDatasetAs (File dataset) "data"
      expr <- open (File file)
      rows <- replicateM iter $
         testWithSetup file expr gconfig { progCxt = ProgCxt r { γ = γ <+> xv } }
            { δv: identity, fwd_expect: mempty, bwd_expect: mempty }
      pure $ averageRows rows

linkMany :: Array TestLinkSpec -> Array (String × Aff Unit)
linkMany specs = zip (specs <#> name) (specs <#> linkOne)
   where
   name spec = "linking/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2
   linkOne { spec, δv1, v2_expect } = do
      { γ0, γ, e1, e2, t1, t2, v1 } <- loadLinkFig spec
      let { v': v2' } = successful $ linkResult spec.x γ0 γ e1 e2 t1 t2 (δv1 v1)
      checkPretty "Linked output" v2_expect v2'
