module Test.Test where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import File (class LoadFile)
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite, TestSuite2)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, BenchSuite2, bwdSuite, suite, withDatasetSuite)
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

nib :: (forall m. LoadFile m => String × m Unit) -> String × Aff Unit
nib quib = second runWebT $ quib

blah :: (forall m. TestSuite2 m) -> Array (Aff (String × Unit))
blah suite = runWebT <$> sequence <$> suite

--blah2 :: (forall m. TestSuite2 m) -> Array (String × Aff Unit)
--blah2 suite = nib <$> suite

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> [] -- linkedOutputsSuite linkedOutputs_cases
   <> [] -- linkedInputsSuite linkedInputs_cases

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
