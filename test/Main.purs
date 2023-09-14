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
import Effect (Effect)
import Effect.Aff (Aff)
import Module (File(..))
import Test.Spec.Specs (bwd_cases, desugar_cases, graphics_cases, linking_cases, misc_cases)
import Test.TestRunners (linkMany, run, testBwdMany, testMany, testWithDatasetMany)
import Util (type (×), (×))

main :: Effect Unit
main = do
   run tests

tests :: Array (String × Aff Unit)
tests = concat
   [ test_desugaring
   , test_misc
   , test_bwd
   , test_graphics
   , test_linking
   ]

{-
tests is_bench = [ test_scratchpad is_bench ]
-}

-- test_scratchpad :: Boolean -> Test Unit
-- test_scratchpad = testBwdMany
--    [ { file: "filter"
--      , file_expect: "filter.expect"
--      , δv: listElementCell 0 # as𝔹Selector
--      , fwd_expect: "(_8_ _:_ (7 : []))"
--      }
--    ]

test_desugaring :: Array (String × Aff Unit)
test_desugaring = testMany desugar_cases

test_misc :: Array (String × Aff Unit)
test_misc = testMany misc_cases

test_bwd :: Array (String × Aff Unit)
test_bwd = testBwdMany bwd_cases

test_graphics :: Array (String × Aff Unit)
test_graphics = testWithDatasetMany graphics_cases

test_linking :: Array (String × Aff Unit)
test_linking = linkMany linking_cases
