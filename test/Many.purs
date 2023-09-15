module Test.Many where

import Prelude

import App.Fig (linkResult, loadLinkFig)
import Benchmark.Util (BenchRow)
import Data.Array (zip)
import Data.List.Lazy (replicateM)
import Effect.Aff (Aff)
import EvalGraph (GraphConfig)
import Graph.GraphImpl (GraphImpl)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports)
import Test.Util (TestBwdSpec, TestLinkSpec, TestSpec, TestWithDatasetSpec, averageRows, checkPretty, testWithSetup)
import Util (type (×), (×), successful)
import Val ((<+>))

many :: Array TestSpec -> Array (String × Aff BenchRow)
many fxs = zip names affs
   where
   affs = map
      ( \{ file, fwd_expect } -> do
           default <- openDefaultImports :: Aff (GraphConfig GraphImpl)
           expr <- open (File file)
           rows <- replicateM 10 $ testWithSetup file expr default { δv: identity, fwd_expect, bwd_expect: mempty }
           pure $ averageRows rows
      )
      fxs
   names = map _.file fxs

bwdMany :: Array TestBwdSpec -> Array (String × Aff BenchRow)
bwdMany fxs = zip names affs
   where
   folder = File "slicing/"
   affs = map
      ( \{ file, file_expect, δv, fwd_expect } -> do
           default <- openDefaultImports :: Aff (GraphConfig GraphImpl)
           bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
           expr <- open (folder <> File file)
           rows <- replicateM 10 $ testWithSetup file expr default { δv, fwd_expect, bwd_expect }
           pure $ averageRows rows
      )
      fxs
   names = map _.file fxs

withDatasetMany :: Array TestWithDatasetSpec -> Array (String × Aff BenchRow)
withDatasetMany fxs = zip names affs
   where
   affs = map
      ( \{ dataset, file } -> do
           default <- openDefaultImports :: Aff (GraphConfig GraphImpl)
           { g, n, γα } × xv <- openDatasetAs (File dataset) "data" default
           let loadedData = { g, n, γα: γα <+> xv }
           expr <- open (File file)
           rows <- replicateM 10 $ testWithSetup file expr loadedData { δv: identity, fwd_expect: mempty, bwd_expect: mempty }
           pure $ averageRows rows
      )
      fxs
   names = map (\spec -> spec.file) fxs

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
