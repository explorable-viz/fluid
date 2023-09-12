module Benchmark.Runners
   ( benchBwdMany
   , benchMany
   , benchWithDatasetMany
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
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports)
import Test.Spec.Assertions (fail)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec, testWithSetup)
import Util ((×))
import Val ((<+>))

------------------------
-- Many's
------------------------

benchMany :: Array TestSpec -> Aff BenchAcc
benchMany fxs = do
   default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
   BenchAcc <$> traverse
      ( \{ file, fwd_expect } -> do
           expr <- open (File file)
           testWithSetup file true expr default { δv: identity, fwd_expect, bwd_expect: mempty }
      )
      fxs

benchBwdMany :: Array TestBwdSpec -> Aff BenchAcc
benchBwdMany fxs = do
   default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
   let folder = File "slicing/"
   BenchAcc <$> traverse
      ( \{ file, file_expect, δv, fwd_expect } -> do
           bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
           expr <- open (folder <> File file)
           testWithSetup file true expr default { δv, fwd_expect, bwd_expect }
      )
      fxs

benchWithDatasetMany :: Array TestWithDatasetSpec -> Aff BenchAcc
benchWithDatasetMany fxs = do
   default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
   BenchAcc <$> traverse
      ( \{ dataset, file } -> do
           { g, n, γα } × xv <- openDatasetAs (File dataset) "data" default
           let loadedData = { g, n, γα: γα <+> xv }
           expr <- open (File file)
           testWithSetup file true expr loadedData { δv: identity, fwd_expect: mempty, bwd_expect: mempty }
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
