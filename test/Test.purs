module Test.Test where

import Prelude hiding (add)

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import File (class LoadFile, FileCxt(..))
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite, fluidSrcPaths)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

main :: Effect Unit
main = run (second (runWebT (FileCxt { fluidSrcPaths })) <$> tests)

--main = run (second (runWebT (FileCxt { fluidSrcPaths })) <$> scratchpad)

scratchpad :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
scratchpad = asTestSuite $ suite
   [ { file: "arithmetic.fld", fwd_expect: "42" } ]

asTestSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => BenchSuite m -> TestSuite m
asTestSuite suite = second void <$> suite (1 × false)

tests :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (BenchSuite m)
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
