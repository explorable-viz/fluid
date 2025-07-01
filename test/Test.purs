module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite, TestSuite2)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, BenchSuite2, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util (type (×), (×))

main :: Effect Unit
main = run tests

-- main = run scratchpad

{-
scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "comments/projection"
     , imports: []
     , fwd_expect: "\"\"\" Test \"\"\" 1"
     }
   ]
-}
asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

asTestSuite2 :: forall m. BenchSuite2 m -> TestSuite2 m
asTestSuite2 suite = second void <$> suite (1 × false)

blah3 :: (forall m. TestSuite2 m) -> Array (Aff (String × Unit))
blah3 suite = runWebT <$> sequence <$> suite

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: Array BenchSuite
benchmarks =
   []
{-
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
-}

benchmarks' :: forall m. Array (BenchSuite2 m)
benchmarks' =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]

testCases :: forall m. Array (TestSuite2 m)
testCases = benchmarks' <#> asTestSuite2
