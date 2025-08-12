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
import Test.Specs.ParagraphComments (paragraph_comments_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

-- ====== pick ONE main ======

-- ① Only run paragraph tests (default)
-- main :: Effect Unit
-- main = run paragraphCommentsTests

-- ② Run everything (uncomment these two lines and comment out the main above)
main :: Effect Unit
main = run allTests

-- ③ Only run the selected 7 comment tests
-- main :: Effect Unit
-- main = run selectedCommentsTests

-- --------------------------------

-- Only the 7 specific comment tests
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

-- Only paragraph tests
paragraphCommentsTests :: TestSuite
paragraphCommentsTests =
   second void <$> suite loadFile paragraph_comments_cases (1 × false)

-- All benchmarks + linked IO tests (paragraph included)
allTests :: TestSuite
allTests =
   concat (benchmarks <#> asTestSuite)
      <> linkedOutputsSuite linkedOutputs_cases
      <> linkedInputsSuite linkedInputs_cases

asTestSuite :: BenchSuite -> TestSuite
asTestSuite mkSuite = second void <$> mkSuite (1 × false)

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , suite loadFile comments_cases
   , suite loadFile paragraph_comments_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]

