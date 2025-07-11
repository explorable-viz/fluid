module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (matrixElement, select)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import File (class LoadFile)
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

main :: Effect Unit
main = run (second runWebT <$> tests)

-- main = run (second runWebT <$> scratchpad)

scratchpad :: forall m. MonadAff m => MonadError Error m => LoadFile m => TestSuite m
scratchpad = asTestSuite $ bwdSuite
   [ { file: "matrix/matmul"
     , imports:
          [ "lib/matrix"
          , "slicing/matrix/a-matrix"
          , "slicing/matrix/b-matrix"
          , "slicing/matrix/c-matrix"
          ]
     , bwd_expect_file: "matrix/matmul.expect"
     , δv: matrixElement 1 1 select
     , fwd_expect: "⸨64⸩, -58,\n154, -139"
     , datasets: []
     }
   ]

asTestSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => BenchSuite m -> TestSuite m
asTestSuite suite = second void <$> suite (1 × false)

tests :: forall m. MonadAff m => MonadError Error m => LoadFile m => TestSuite m
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array (BenchSuite m)
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
