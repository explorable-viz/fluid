module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (matrixElement, select)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Module.Web (loadFile)
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
main = run tests

-- main = run $ asTestSuite (suite loadFile comments_cases)

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite loadFile
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

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , suite loadFile comments_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]
