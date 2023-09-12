module Benchmark.Runners
   ( benchMany
   , shouldSatisfy
   ) where

import Prelude hiding (absurd)

import Benchmark.Util (BenchAcc(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Set (Set) as S
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import EvalGraph (GraphConfig)
import Graph.GraphImpl (GraphImpl)
import Module (File(..), open, openDefaultImports)
import Test.Spec.Assertions (fail)
import Test.Util (TestSpec, testWithSetup)

------------------------
-- Many's
------------------------

benchMany :: Array TestSpec -> Aff BenchAcc
benchMany fxs = do
   default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
   BenchAcc <$> traverse
      ( \{ file, fwd_expect } -> do
           config <- open (File file)
           testWithSetup file true config default { δv: identity, fwd_expect, bwd_expect: mempty }
      )
      fxs

------------------------
-- Assertions
------------------------
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg
