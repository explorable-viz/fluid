module Test.Main where

import Prelude hiding (add)

import App.Util.Select (listElement)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Aff)
import Lattice (neg)
import Test.Specs (bwd_cases, desugar_cases, graphics_cases, linking_cases, misc_cases)
import Test.Util.Many (bwdMany, linkMany, many, withDatasetMany)
import Test.Util.Mocha (run)
import Util (type (×), (×))

main :: Effect Unit
main = run tests

tests :: Array (String × Aff Unit)
tests = concat
   [ test_desugaring
   , test_misc
   , test_bwd
   , test_graphics
   , test_linking
   ]
{-
tests = concat [ test_scratchpad ]
-}

test_scratchpad :: Array (String × Aff Unit)
test_scratchpad = second void <$> bwdMany
   [ { file: "dtw/compute-dtw"
     , file_expect: "dtw/compute-dtw.expect"
     , fwd_expect: "((1, 1) : (⸨(⸨2⸩, ⸨2⸩)⸩ : ((2, 3) : ((3, 4) : ((4, 5) : ((5, 6) : ((5, 7) : [])))))))"
     , δv: listElement 1 neg
     }
   ]
   (1 × false)

test_desugaring :: Array (String × Aff Unit)
test_desugaring = second void <$> many desugar_cases (1 × false)

test_misc :: Array (String × Aff Unit)
test_misc = second void <$> many misc_cases (1 × false)

test_bwd :: Array (String × Aff Unit)
test_bwd = second void <$> bwdMany bwd_cases (1 × false)

test_graphics :: Array (String × Aff Unit)
test_graphics = second void <$> withDatasetMany graphics_cases (1 × false)

test_linking :: Array (String × Aff Unit)
test_linking = linkMany linking_cases
