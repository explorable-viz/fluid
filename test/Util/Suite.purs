module Test.Util.Many where

import Prelude

import App.Fig (LinkedInputsFigSpec, LinkedOutputsFigSpec, linkedInputsResult, linkedOutputsResult, loadLinkedInputsFig, loadLinkedOutputsFig)
import Data.Array (zip)
import Data.Either (Either(..))
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

suite :: Array TestSpec -> BenchSuite
suite specs (n × is_bench) = zip (specs <#> _.file) (specs <#> asTest)
   where
   asTest :: TestSpec -> Aff BenchRow
   asTest { file, fwd_expect } = do
      progCxt <- defaultImports
      test (File file) progCxt { δv: identity, fwd_expect, bwd_expect: mempty } (n × is_bench)

bwdSuite :: Array TestBwdSpec -> BenchSuite
bwdSuite specs (n × is_bench) = zip (specs <#> (_.file >>> ("slicing/" <> _))) (specs <#> asTest)
   where
   folder = File "slicing/"

   asTest :: TestBwdSpec -> Aff BenchRow
   asTest { file, bwd_expect_file, δv, fwd_expect } = do
      progCxt <- defaultImports
      bwd_expect <- loadFile (Folder "fluid/example") (folder <> File bwd_expect_file)
      test (folder <> File file) progCxt { δv, fwd_expect, bwd_expect } (n × is_bench)

withDatasetSuite :: Array TestWithDatasetSpec -> BenchSuite
withDatasetSuite specs (n × is_bench) = zip (specs <#> _.file) (specs <#> asTest)
   where
   asTest :: TestWithDatasetSpec -> Aff BenchRow
   asTest { dataset, file } = do
      progCxt <- defaultImports >>= datasetAs (File dataset) "data"
      test (File file) progCxt { δv: identity, fwd_expect: mempty, bwd_expect: mempty } (n × is_bench)

linkedOutputsTest :: TestLinkedOutputsSpec -> Aff Unit
linkedOutputsTest { spec, δv1, v2_expect } = do
   fig <- loadLinkedOutputsFig spec
   _ × v2' × _ <- linkedOutputsResult fig (Left δv1)
   checkPretty "linked output" v2_expect v2'

linkedOutputsSuite :: Array TestLinkedOutputsSpec -> Array (String × Aff Unit)
linkedOutputsSuite specs = zip (specs <#> name) (specs <#> linkedOutputsTest)
   where
   name spec = "linked-outputs/" <> show spec.spec.file1 <> "<->" <> show spec.spec.file2

linkedInputsTest :: TestLinkedInputsSpec -> Aff Unit
linkedInputsTest { spec, δv1, v2_expect } = do
   { γ, e, t } <- loadLinkedInputsFig spec
   { v': v2' } <- linkedInputsResult spec.x1 spec.x2 γ e t δv1
   checkPretty "linked input" v2_expect v2'

linkedInputsSuite :: Array TestLinkedInputsSpec -> Array (String × Aff Unit)
linkedInputsSuite specs = zip (specs <#> name) (specs <#> linkedInputsTest)
   where
   name { spec } = "linked-inputs/" <> show spec.file
