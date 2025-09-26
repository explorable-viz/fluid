module Test.Test where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Array (concat, filter, elem)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import File (class LoadFile, FileCxt(..))
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Specs.Paragraph (paragraph_cases)
import Test.Util (TestSuite, fluidSrcPaths)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, TestSpec, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite)
import Util ((×))

main :: Effect Unit
main = run (second (runWebT (FileCxt { fluidSrcPaths })) <$> filterSuite files comments_cases)
   where
   files =
      [ "comments/nested-constr.fld"
      -- , "comments/dicts.fld"
      -- , "comments/app-arg.fld"
      -- , "comments/list-comp.fld"
      -- , "comments/app.fld"
      -- , "comments/int.fld"
      -- , "comments/projection.fld"
      ]

scratchpad :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
scratchpad = second void <$> suite paragraph_cases (1 × false)

filterSuite :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array String -> Array TestSpec -> TestSuite m
filterSuite files cases =
   second void <$> suite (filter (\c -> c.file `elem` files) cases) (1 × false)

allTests :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
allTests =
   concat (benchmarks <#> asTestSuite)
      <> linkedOutputsSuite linkedOutputs_cases
      <> linkedInputsSuite linkedInputs_cases

asTestSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => BenchSuite m -> TestSuite m
asTestSuite suite = second void <$> suite (1 × false)

benchmarks :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (BenchSuite m)
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , suite paragraph_cases
   , bwdSuite bwd_cases
   , suite graphics_cases
   ]
