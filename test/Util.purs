module Test.Util where

import Prelude hiding (absurd)

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List (elem)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import DataType (dataTypeFor, typeName)
import Desug (desugGC)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import GaloisConnection (GaloisConnection(..), dual)
import Graph.GraphImpl (GraphImpl)
import Lattice (Raw, 𝔹, botOf, erase, expand, topOf)
import Module (File, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, logAs, recordGraphSize)
import Test.Spec.Assertions (fail)
import Test.Util.Debug (checking, debug)
import Util (AffError, EffectError, Thunk, type (×), (×), check, successful)
import Val (class Ann, BaseVal(..), Val(..))

type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state

type SelectionSpec =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

test ∷ forall m. File -> GraphConfig GraphImpl -> SelectionSpec -> Int × Boolean -> AffError m BenchRow
test file gconfig spec (n × benchmarking) = do
   s <- open file
   testPretty s
   _ × row_accum <- runWriterT
      ( replicateM n $ do
           testTrace s gconfig spec
           testGraph s gconfig spec benchmarking
      )
   pure $ row_accum `divRow` n

testPretty :: forall m a. Ann a => SE.Expr a -> AffError m Unit
testPretty s = do
   s' <- parse (prettyP s) program
   unless (eq (erase s) (erase s')) do
      logAs "Original" $ show (erase s)
      logAs "New" $ show (erase s')
      fail "parse/prettyP round trip"

checkPretty :: forall a m. Pretty a => String -> String -> a -> EffectError m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x) $ do
      logAs "\nExpected" $ "\n" <> expect
      logAs "\nReceived" $ "\n" <> prettyP x
      fail msg

validate :: forall m. String -> SelectionSpec -> SE.Expr 𝔹 -> Val 𝔹 -> EffectError m Unit
validate method { bwd_expect, fwd_expect } s𝔹 v𝔹 = do
   unless (null bwd_expect) $
      checkPretty (method <> "-based bwd_expect") bwd_expect s𝔹
   unless (isGraphical v𝔹) do
      when debug.logging $ logAs (method <> "-based fwd ⚬ bwd") (prettyP v𝔹)
      checkPretty (method <> "-based fwd_expect") fwd_expect v𝔹

traceMethod :: String
traceMethod = "T"

traceBenchmark :: forall m a. MonadWriter BenchRow m => String -> Thunk (m a) -> EffectError m a
traceBenchmark name = benchmark (traceMethod <> "-" <> name)

graphMethod :: String
graphMethod = "G"

graphBenchmark :: forall m a. MonadWriter BenchRow m => String -> (Unit -> m a) -> EffectError m a
graphBenchmark name = benchmark (graphMethod <> "-" <> name)

testTrace :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> SelectionSpec -> AffError m Unit
testTrace s gconfig spec@{ δv } = do
   { gc: GC eval, v } <- do
      { gc: GC desug } <- desugGC s
      let
         e = desug.fwd s
         γ = erase <$> gconfig.γ
      traceBenchmark "Eval" $ \_ -> traceGC γ e

   let v𝔹 = δv (botOf v)
   γ𝔹 × e𝔹 <- do
      when debug.logging (logAs "Selection for bwd" (prettyP v𝔹))
      traceBenchmark "Bwd" $ \_ -> pure (eval.bwd v𝔹)

   { gc: GC desug𝔹, e } <- desugGC s
   let s𝔹 = desug𝔹.bwd e𝔹
   v𝔹' <- do
      let e𝔹' = desug𝔹.fwd s𝔹
      PrettyShow e𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (desugar)"` (unwrap >>> (_ >= expand e𝔹 e))
      traceBenchmark "Fwd" $ \_ -> pure (eval.fwd (γ𝔹 × e𝔹'))
   PrettyShow v𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (eval)"` (unwrap >>> (_ >= v𝔹))

   let
      v𝔹_top = topOf v
      γ𝔹_top × e𝔹_top = eval.bwd v𝔹_top
      s𝔹_top = desug𝔹.bwd e𝔹_top
      e𝔹_top' = desug𝔹.fwd s𝔹_top
      v𝔹_top' = eval.fwd (γ𝔹_top × e𝔹_top')
   PrettyShow v𝔹_top' `shouldSatisfy "fwd ⚬ bwd round-trip (eval ⚬ desugar)"` (unwrap >>> (_ >= v𝔹_top))

   validate traceMethod spec s𝔹 v𝔹'

testGraph :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> SelectionSpec -> Boolean -> AffError m Unit
testGraph s gconfig spec@{ δv } _ = do

   { gc: gc@(GC eval), gc_op: GC eval_op, g, vα } <- do
      { gc: GC desug } <- desugGC s
      let e = desug.fwd s
      graphBenchmark "Eval" $ \_ -> graphGC gconfig e

   let v𝔹 = δv (botOf vα)
   γ𝔹 × e𝔹 <- graphBenchmark "Bwd" $ \_ -> pure (eval.bwd v𝔹)
   v𝔹' <- graphBenchmark "Fwd" $ \_ -> pure (eval.fwd (γ𝔹 × e𝔹))

   { gc: GC desug𝔹 } <- desugGC s
   validate graphMethod spec (desug𝔹.bwd e𝔹) v𝔹'
   PrettyShow v𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (eval)"` (unwrap >>> (_ >= v𝔹))
   recordGraphSize g

   let eval_dual = unwrap (dual gc)
   in1 <- graphBenchmark "BwdDlFwdOp" $ \_ -> pure (eval_op.fwd v𝔹)
   in2 <- graphBenchmark "BwdDlCmp" $ \_ -> pure (eval_dual.fwd v𝔹)
   when checking.bwdDuals $
      check (in1 == in2) "Two constructions of bwd dual agree"
   void $ graphBenchmark "BwdAll" $ \_ -> pure (eval.bwd (topOf vα))

   out1 <- graphBenchmark "FwdDlBwdOp" $ \_ -> pure (eval_op.bwd (γ𝔹 × e𝔹))
   out2 <- graphBenchmark "FwdDlCmp" $ \_ -> pure (eval_dual.bwd (γ𝔹 × e𝔹))
   when checking.fwdDuals $
      check (out1 == out2) "Two constructions of fwd dual agree"

   out3 <- benchmark "Naive-Fwd" $ \_ -> pure ((unwrap (dual (GC eval_op))).fwd (γ𝔹 × e𝔹))
   when checking.naiveFwd $
      check (out1 == out3) "Naive and direct fwd agree"
   pure unit

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Val _ (Constr c _)) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement" ]
isGraphical _ = false

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)
