module Benchmark.BenchRunners
   ( benchBwdMany
   , benchMany
   , benchWithDatasetMany
   , shouldSatisfy
   ) where

import Prelude hiding (absurd)

import Benchmark.Util (BenchRow)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Test.Many (bwdMany, withDatasetMany, many)
import Test.Spec.Assertions (fail)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec)
import Util (type (×))

------------------------
-- Many's
------------------------

benchMany :: Array TestSpec -> Array (String × Aff BenchRow)
benchMany = many true

benchBwdMany :: Array TestBwdSpec -> Array (String × Aff BenchRow)
benchBwdMany = bwdMany true

benchWithDatasetMany :: Array TestWithDatasetSpec -> Array (String × Aff BenchRow)
benchWithDatasetMany = withDatasetMany true

------------------------
-- Assertions
------------------------
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg
