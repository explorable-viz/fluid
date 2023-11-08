module Test.Util.Many where

import Prelude

import App.Fig (LinkedInputsFigSpec, LinkedOutputsFigSpec, linkedOutputsResult, loadLinkedInputsFig, loadLinkedOutputsFig)
import Data.Array (zip)
import Effect.Aff (Aff)
import Module (File(..), Folder(..), datasetAs, defaultImports, loadFile)
import Test.Benchmark.Util (BenchRow)
import Test.Util (Selector, checkPretty, test)
import Util (type (×), (×))
import Val (Val)

-- benchmarks parameterised on number of iterations
type BenchSuite = (Int × Boolean) -> Array (String × Aff BenchRow)

type TestSpec =
   { file :: String
   , fwd_expect :: String
   }

type TestBwdSpec =
   { file :: String
   , bwd_expect_file :: String
   , δv :: Selector Val -- relative to bot
   , fwd_expect :: String
   }

type TestWithDatasetSpec =
   { dataset :: String
   , file :: String
   }

type TestLinkedOutputsSpec =
   { spec :: LinkedOutputsFigSpec
   , δv1 :: Selector Val
   , v2_expect :: String
   }

type TestLinkedInputsSpec =
   { spec :: LinkedInputsFigSpec
   , δv1 :: Selector Val
   , v2_expect :: String
   }

many :: Array TestSpec -> BenchSuite
many specs (n × is_bench) = zip (specs <#> _.file) (specs <#> one)
   where
   one { file, fwd_expect } = do
      progCxt <- defaultImports
      test (File file) progCxt { δv: identity, fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdMany :: Array TestBwdSpec -> BenchSuite
bwdMany specs (n × is_bench) = zip (specs <#> (\spec -> "slicing/" <> spec.file)) (specs <#> one)
   where
   folder = File "slicing/"
   one { file, bwd_expect_file, δv, fwd_expect } = do
      progCxt <- defaultImports
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File bwd_expect_file)
      test (folder <> File file) progCxt { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetMany :: Array TestWithDatasetSpec -> BenchSuite
withDatasetMany specs (n × is_bench) = zip (specs <#> _.file) (specs <#> one)
   where
   one { dataset, file } = do
      progCxt <- defaultImports >>= datasetAs (File dataset) "data"
      test (File file) progCxt { δv: identity, fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsMany :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsMany specs = zip (specs <#> name) (specs <#> one)
   where
   name spec = "linked-outputs/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2
   one { spec, δv1, v2_expect } = do
      { γ, e1, e2, t1, t2, v1 } <- loadLinkedOutputsFig spec
      { v': v2' } <- linkedOutputsResult spec.x γ e1 e2 t1 t2 (δv1 v1)
      checkPretty "linked output" v2_expect v2'

linkedInputsMany :: Array TestLinkedInputsSpec -> Array (String × Aff Unit)
linkedInputsMany specs = zip (specs <#> name) (specs <#> one)
   where
   name { spec } = "linked-inputs/" <> show spec.file
   one { spec } = do
      {} <- loadLinkedInputsFig spec
      pure unit
