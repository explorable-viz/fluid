module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec)
import App.Util (Selector)
import Benchmark.Util (BenchRow(..), GraphRow, TraceRow, zeroRow, sumRow, preciseTime, tdiff)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (elem)
import Data.List.Lazy (List, length)
import Data.Set (subset)
import Data.String (null)
import DataType (dataTypeFor, typeName)
import Debug (trace)
import Desugarable (desug, desugBwd)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import Graph (sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDual, fwdSliceDual, fwdSliceDeMorgan) as G
import Heterogeneous.Mapping (hmap)
import Lattice (botOf, topOf, erase, Raw)
import Module (parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (MayFailT, successful)
import Val (Val(..), class Ann)

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
-- testWithSetup :: Boolean -> SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff BenchRow
testWithSetup ∷ String -> SE.Expr Unit → GraphConfig GraphImpl → TestConfig → Aff BenchRow
testWithSetup _name s gconfig tconfig =
   liftEither =<<
      ( runExceptT $ do
           testParse s
           trRow <- testTrace s gconfig tconfig
           grRow <- testGraph s gconfig tconfig
           pure $ BenchRow trRow grRow
      )

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src) \_ ->
      unless (eq (erase s) (erase s')) do
         log ("SRC\n" <> show (erase s))
         log ("NEW\n" <> show (erase s'))
         lift $ fail "not equal"

testTrace :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff TraceRow
testTrace s { γα } { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   tEval1 <- preciseTime
   gc <- traceGC (erase <$> γα) e
   tEval2 <- preciseTime

   -- | Backward
   tBwd1 <- preciseTime
   let { γ: γ𝔹, e: e𝔹 } = gc.bwd (δv (botOf gc.v))
   tBwd2 <- preciseTime
   let s𝔹 = desugBwd e𝔹 s

   -- | Forward (round-tripping)
   e𝔹' <- desug s𝔹
   tFwd1 <- preciseTime
   let v𝔹 = gc.fwd { γ: γ𝔹, e: e𝔹', α: top }
   tFwd2 <- preciseTime

   lift do
      unless (isGraphical gc.v) $
         log (prettyP v𝔹)
      -- | Check backward selections
      unless (null bwd_expect) $
         checkPretty "Trace-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical gc.v) $
         checkPretty "Trace-based value" fwd_expect v𝔹

   pure { tEval: tdiff tEval1 tEval2, tBwd: tdiff tBwd1 tBwd2, tFwd: tdiff tFwd1 tFwd2 }

testGraph :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff GraphRow
testGraph s gconfig { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   tEval1 <- preciseTime
   gc <- graphGC gconfig e
   tEval2 <- preciseTime

   -- | Backward
   tBwd1 <- preciseTime
   let
      αs_out = gc.runδv δv
      αs_in = gc.bwd αs_out
      e𝔹 = gc.selecte𝔹 αs_in
   tBwd2 <- preciseTime
   let s𝔹 = desugBwd e𝔹 s

   -- | De Morgan dual of backward
   tBwdDual1 <- preciseTime
   let
      αs_out_dual = gc.runδv δv
      gbwd_dual = G.bwdSliceDual αs_out_dual gc.g
      αs_in_dual = sinks gbwd_dual
      e𝔹_dual = gc.selecte𝔹 αs_in_dual
   tBwdDual2 <- preciseTime

   -- | Backward (all outputs selected)
   tBwdAll1 <- preciseTime
   let
      αs_out_all = gc.runδv topOf
      αs_in_all = gc.bwd αs_out_all
      e𝔹_all = gc.selecte𝔹 αs_in_all
   tBwdAll2 <- preciseTime

   -- | Forward (round-tripping)
   tFwd1 <- preciseTime
   let
      αs_out' = gc.fwd αs_in
      v𝔹 = gc.selectv𝔹 αs_out'
   tFwd2 <- preciseTime

   -- | De Morgan dual of forward
   tFwdDual1 <- preciseTime
   let
      gfwd_dual = G.fwdSliceDual αs_in gc.g
      v𝔹_dual = gc.selectv𝔹 (vertices gfwd_dual)
   tFwdDual2 <- preciseTime

   -- | Forward (round-tripping) using De Morgan dual
   tFwdAsDeMorgan1 <- preciseTime
   let
      gfwd_demorgan = G.fwdSliceDeMorgan αs_in gc.g
      v𝔹_demorgan = gc.selectv𝔹 (vertices gfwd_demorgan) <#> not
   tFwdAsDeMorgan2 <- preciseTime

   lift do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
         checkPretty "Graph-based value (De Morgan)" fwd_expect v𝔹_demorgan
      αs_out `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset αs_out')
      -- | To avoid unused variables when benchmarking
      unless false do
         log (prettyP e𝔹_dual)
         log (prettyP e𝔹_all)
         log (prettyP v𝔹_dual)

   pure { tEval: tdiff tEval1 tEval2, tBwd: tdiff tBwd1 tBwd2, tBwdDual: tdiff tBwdDual1 tBwdDual2, tBwdAll: tdiff tBwdAll1 tBwdAll2, tFwd: tdiff tFwd1 tFwd2, tFwdDual: tdiff tFwdDual1 tFwdDual2, tFwdAsDemorgan: tdiff tFwdAsDeMorgan1 tFwdAsDeMorgan2 }

type TestSpec =
   { file :: String
   , fwd_expect :: String
   }

type TestBwdSpec =
   { file :: String
   , file_expect :: String
   , δv :: Selector Val -- relative to bot
   , fwd_expect :: String
   }

type TestWithDatasetSpec =
   { dataset :: String
   , file :: String
   }

type TestLinkSpec =
   { spec :: LinkFigSpec
   , δv1 :: Selector Val
   , v2_expect :: String
   }

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

checkPretty :: forall a m. MonadThrow Error m => Pretty a => String -> String -> a -> m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x) $
      fail (msg <> "\nExpected:\n" <> expect <> "\nReceived:\n" <> prettyP x)

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)

averageRows :: List BenchRow -> BenchRow
averageRows rows = averagedTr
   where
   runs = toNumber $ length rows

   summed = foldl sumRow zeroRow rows
   averagedTr = (\(BenchRow tr gr) -> BenchRow (hmap (\num -> num `div` runs) tr) (hmap (\num -> num `div` runs) gr)) $ summed
