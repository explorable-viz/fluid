module Test.Test where

import Prelude

import Data.Array (concat, filter, elem)
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
import Test.Specs.ParagraphComments (paragraph_comments_cases) -- ← Added
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

-- === ✅ Currently enabled: only run selected comment test cases ===
-- main :: Effect Unit
-- main = run selectedCommentsTests

-- === ❌ Uncomment to run all tests (including paragraph cases) ===
-- main :: Effect Unit
-- main = run allTests

-- === ❌ Uncomment to run all comment cases (not just 7 selected ones) ===
-- main :: Effect Unit
-- main = run (second void <$> suite loadFile comments_cases (1 × false))

-- === ❌ Uncomment to run only paragraph comment cases ===
main :: Effect Unit
main = run paragraphCommentsTests

--------------------------------------------------------------------------------
-- ✅ Run only these 7 specific comment tests
selectedCommentsTests :: TestSuite
selectedCommentsTests = second void <$> suite loadFile selectedCases (1 × false)
   where
   selectedNames =
      [ "comments/nested-constr"
      , "comments/dicts"
      , "comments/app-arg"
      , "comments/list-comp"
      , "comments/app"
      , "comments/int"
      , "comments/projection"
      ]
   selectedCases = filter (\c -> c.file `elem` selectedNames) comments_cases

--------------------------------------------------------------------------------
-- ✅ Run only paragraph comment test cases
paragraphCommentsTests :: TestSuite
paragraphCommentsTests = second void <$> suite loadFile paragraph_comments_cases (1 × false)

--------------------------------------------------------------------------------
-- ✅ Run all benchmark and linked input/output tests (paragraph cases included)
allTests :: TestSuite
allTests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , suite loadFile comments_cases
   , suite loadFile paragraph_comments_cases -- ← Added paragraph cases here
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]

