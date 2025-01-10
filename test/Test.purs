module Test.Test where

import Prelude hiding (add)

import Bind ((↦))
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Module.Web (loadFile)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite)
import Util ((×))
import Website.Benchmark (benchmarks)

main :: Effect Unit
main = run tests

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite loadFile
   [ { file: "qcut"
     , imports:
          [ "lib/stats"
          ]
     , bwd_expect_file: "qcut.expect"
     , δv: identity
     , fwd_expect: "(((1.01 : (1.05 : [])), 0.051000000000000156) : (((1.07 : (1.09 : (1.22 : (1.23 : (1.24 : (1.24 : (1.25 : (1.32 : (1.32 : (1.35 : (1.3900000000000001 : (1.47 : (1.57 : (1.72 : [])))))))))))))), 0.6639999999999999) : (((1.73 : (1.75 : (1.76 : (1.83 : (1.8699999999999999 : (1.94 : (2.04 : (2.14 : (2.18 : (2.36 : (2.37 : (2.38 : (2.52 : (2.54 : [])))))))))))))), 0.8464999999999998) : (((2.61 : (2.67 : [])), 0.09850000000000003) : []))))"
     , datasets: [ "ssp126" ↦ "dataset/ssp126-2081-2100" ]
     }
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
