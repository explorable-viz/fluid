module Test.Util2
  ( benchMany
  , shouldSatisfy
  )
  where

import Prelude hiding (absurd)

import App.Util (Selector)
import Benchmark.Util (BenchRow(..), GraphRow, TraceRow, bench, BenchAcc(..))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExceptT, except)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Set (Set) as S
import Data.String (null)
import Data.Traversable (traverse)
import DataType (dataTypeFor, typeName)
import Debug (trace)
import Desugarable (desug, desugBwd)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalGraph (GraphConfig, evalWithConfig)
import Graph (sinks, vertices, sources)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice, fwdSliceDeMorgan) as G
import Graph.Slice (selectαs, select𝔹s)
import Lattice (erase, botOf, bot)
import Module (File(..), open, openDefaultImports, parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Set (subset)
import Test.Spec.Assertions (fail)
import Test.Util (TestConfig, TestSpec, testWithSetup)
import Util (MayFailT, (×), successful, error)
import Val (Val(..), class Ann)

------------------------
-- Many's
------------------------

benchMany :: Array TestSpec -> Aff BenchAcc
benchMany fxs = do
   default <- openDefaultImports :: Aff (GraphConfig (GraphImpl S.Set))
   BenchAcc <$> traverse (\{file, fwd_expect} -> do
         config <- open (File file)
         testWithSetup file true config default {  δv: identity, fwd_expect, bwd_expect: mempty}
   ) fxs
  
------------------------
-- Assertions
------------------------
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg
