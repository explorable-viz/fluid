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
import Val ((<+>))

many :: Array TestSpec -> Int -> Array (String × Aff BenchRow)
many fxs iter = zip names affs
   where
   affs = fxs <#> \{ file, fwd_expect } -> do
      gconfig <- openDefaultImports
      expr <- open (File file)
      rows <- replicateM iter $
         testWithSetup file expr gconfig { δv: identity, fwd_expect, bwd_expect: mempty }
      pure $ averageRows rows
   names = map _.file fxs

bwdMany :: Array TestBwdSpec -> Int -> Array (String × Aff BenchRow)
bwdMany fxs iter = zip names affs
   where
   folder = File "slicing/"
   affs = fxs <#> \{ file, file_expect, δv, fwd_expect } -> do
      gconfig <- openDefaultImports
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
      expr <- open (folder <> File file)
      rows <- replicateM iter $
         testWithSetup file expr gconfig { δv, fwd_expect, bwd_expect }
      pure $ averageRows rows
   names = map _.file fxs

withDatasetMany :: Array TestWithDatasetSpec -> Int -> Array (String × Aff BenchRow)
withDatasetMany fxs iter = zip names affs
   where
   affs = fxs <#> \{ dataset, file } -> do
      { g, n, γα } × xv <- openDefaultImports >>= openDatasetAs (File dataset) "data"
      let loadedData = { g, n, γα: γα <+> xv }
      expr <- open (File file)
      rows <- replicateM iter $
         testWithSetup file expr loadedData { δv: identity, fwd_expect: mempty, bwd_expect: mempty }
      pure $ averageRows rows
   names = fxs <#> _.file

linkMany :: Array TestLinkSpec -> Array (String × Aff Unit)
linkMany fxs = zip names affs
   where
   names = fxs <#> \spec -> "linking/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2
   affs = fxs <#> \{ spec, δv1, v2_expect } -> do
      { γ0, γ, e1, e2, t1, t2, v1 } <- loadLinkFig spec
      let { v': v2' } = successful $ linkResult spec.x γ0 γ e1 e2 t1 t2 (δv1 v1)
      checkPretty "Linked output" v2_expect v2'
