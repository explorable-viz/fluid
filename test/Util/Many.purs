module Test.Util.Many where

import Prelude
import App.Fig (linkedOutputsResult, loadLinkedOutputsFig)
import Data.Array (zip)
import Effect.Aff (Aff)
import Module (File(..), Folder(..), datasetAs, defaultImports, loadFile)
import Test.Benchmark.Util (BenchRow)
import Test.Util (TestBwdSpec, TestLinkedOutputsSpec, TestSpec, TestWithDatasetSpec, checkPretty, test)
import Util (type (×), (×))

many :: Array TestSpec -> (Int × Boolean) -> Array (String × Aff BenchRow)
many specs (n × is_bench) = zip (specs <#> _.file) (specs <#> one)
   where
   one { file, fwd_expect } = do
      progCxt <- defaultImports
      test (File file) progCxt { δv: identity, fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdMany :: Array TestBwdSpec -> (Int × Boolean) -> Array (String × Aff BenchRow)
bwdMany specs (n × is_bench) = zip (specs <#> (\spec -> "slicing/" <> spec.file)) (specs <#> one)
   where
   folder = File "slicing/"
   one { file, bwd_expect_file, δv, fwd_expect } = do
      progCxt <- defaultImports
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File bwd_expect_file)
      test (folder <> File file) progCxt { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetMany :: Array TestWithDatasetSpec -> (Int × Boolean) -> Array (String × Aff BenchRow)
withDatasetMany specs (n × is_bench) = zip (specs <#> _.file) (specs <#> one)
   where
   one { dataset, file } = do
      progCxt <- defaultImports >>= datasetAs (File dataset) "data"
      test (File file) progCxt { δv: identity, fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsMany :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsMany specs = zip (specs <#> name) (specs <#> one)
   where
   name spec = "linking/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2
   one { spec, δv1, v2_expect } = do
      { γ, e1, e2, t1, t2, v1 } <- loadLinkedOutputsFig spec
      { v': v2' } <- linkedOutputsResult spec.x γ e1 e2 t1 t2 (δv1 v1)
      checkPretty "LinkedOutputsed output" v2_expect v2'
