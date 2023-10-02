module Test.Main
   ( main
   , test_bwd
   , test_desugaring
   , test_graphics
   --  , test_linking
   , test_misc
   --  , test_scratchpad
   ) where

import Prelude hiding (add)

import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Many (bwdMany, linkMany, many, withDatasetMany)
import Test.Spec.Mocha (run)
import Test.Spec.Specs (bwd_cases, desugar_cases, graphics_cases, linking_cases, misc_cases)
import Util (type (×))
import Test.Benchmark.ExperimentEffectNow (benchFibs)

main :: Effect Unit
main = do
   if false then run tests else benchFibs

tests :: Array (String × Aff Unit)
tests = concat
   [ test_desugaring
   , test_misc
   , test_bwd
   , test_graphics
   , test_linking
   ]

-- test_scratchpad :: Array (String × Aff Unit)
-- test_scratchpad = []

test_desugaring :: Array (String × Aff Unit)
test_desugaring = second void <$> many desugar_cases 1

test_misc :: Array (String × Aff Unit)
test_misc = second void <$> many misc_cases 1

test_bwd :: Array (String × Aff Unit)
test_bwd = second void <$> bwdMany bwd_cases 1

test_graphics :: Array (String × Aff Unit)
test_graphics = second void <$> withDatasetMany graphics_cases 1

test_linking :: Array (String × Aff Unit)
test_linking = linkMany linking_cases
