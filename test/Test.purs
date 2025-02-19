module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (matrixElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Lattice (neg)
import Module.Web (loadFile)
import Test.Specs.Bwd (bwd_cases)
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

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite loadFile
   [ { file: "convolution/edgeDetect"
     , imports:
          [ "lib/convolution"
          , "slicing/convolution/filter/edge-detect"
          , "slicing/convolution/test-image"
          ]
     , bwd_expect_file: "convolution/edgeDetect.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨0⸩, -1, 2, 0, -1,\n\
          \0, 3, -2, 3, -2,\n\
          \-1, 1, -5, 0, 4,\n\
          \1, -1, 4, 0, -4,\n\
          \1, 0, -3, 2, 0"
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
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]
