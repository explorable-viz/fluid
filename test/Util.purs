module Test.Util where

import Prelude hiding ((-), absurd)

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (fst, snd)
import Desug (Desugaring, desugGC)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import GaloisConnection (GaloisConnection(..), (***), dual)
import Lattice (class BotOf, class MeetSemilattice, class Neg, Raw, 𝔹, botOf, erase, topOf, (-))
import Module (File, initialConfig, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import ProgCxt (ProgCxt)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, logAs, recordGraphSize)
import Test.Spec.Assertions (fail)
import Test.Util.Debug (testing, tracing)
import Util (type (×), AffError, EffectError, Thunk, check, debug, spyWhen, (×))
import Val (class Ann, Val)

type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state

type SelectionSpec =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

test ∷ forall m. File -> Raw ProgCxt -> SelectionSpec -> Int × Boolean -> AffError m BenchRow
test file progCxt spec (n × _) = do
   s <- open file
   { e } :: Desugaring Unit <- desugGC s
   gconfig <- initialConfig e progCxt
   testPretty s
   _ × row_accum <- runWriterT (replicateM n (testProperties s gconfig spec))
   pure $ row_accum `divRow` n

traceBenchmark :: forall m a. MonadWriter BenchRow m => String -> Thunk (m a) -> EffectError m a
traceBenchmark name = benchmark ("T" <> "-" <> name)

graphBenchmark :: forall m a. MonadWriter BenchRow m => String -> (Unit -> m a) -> EffectError m a
graphBenchmark name = benchmark ("G" <> "-" <> name)

benchNames
   :: { eval :: String
      , bwd :: String
      , fwd :: String
      , bwdDlFwdOp :: String
      , bwdDlCmp :: String
      , bwdAll :: String
      , naiveFwd :: String
      , fwdDlBwdOp :: String
      , fwdDlCmp :: String
      }

benchNames =
   { eval: "Eval"
   , bwd: "Bwd"
   , fwd: "Fwd"
   , bwdDlFwdOp: "BwdDlFwdOp"
   , bwdDlCmp: "BwdDlCmp"
   , bwdAll: "BwdAll"
   , naiveFwd: "Naive-Fwd"
   , fwdDlBwdOp: "FwdDlBwdOp"
   , fwdDlCmp: "FwdDlCmp"
   }

testProperties :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig -> SelectionSpec -> AffError m Unit
testProperties s gconfig { δv, bwd_expect, fwd_expect } = do
   let γ = erase <$> gconfig.γ
   { gc: GC desug, e } <- desugGC s
   { gc: GC evalT, v } <- traceBenchmark benchNames.eval $ \_ ->
      traceGC γ e
   { gc: GC evalG, gc_op: GC evalG_op, g, vα } <- graphBenchmark benchNames.eval $ \_ ->
      graphGC gconfig e

   let out0 = δv (botOf v)
   in_e <- do
      when debug.logging (logAs "Selection for bwd" (prettyP out0))
      traceBenchmark benchNames.bwd $ \_ -> pure (evalT.bwd out0)

   let GC desug' = identity *** (GC desug)
   let in_s = desug'.bwd in_e
   out0' <- do
      let in0' = desug'.fwd in_s
      PrettyShow in0' `shouldSatisfy "fwd ⚬ bwd round-trip (desugar)"` (unwrap >>> (_ >= in_e))
      traceBenchmark benchNames.fwd $ \_ -> pure (evalT.fwd in0')
   PrettyShow out0' `shouldSatisfy "fwd ⚬ bwd round-trip (eval)"` (unwrap >>> (_ >= out0))

   let in_top = topOf (fst in_e) × topOf (snd in_e) -- doesn't lift to pairs as intended
   let out_top = evalT.fwd in_top
   when testing.fwdPreservesTop $
      PrettyShow out_top `shouldSatisfy "trace fwd preserves ⊤"` (unwrap >>> (_ == topOf v))

   -- empty string somewhat hacky encoding for "don't care"
   unless (null bwd_expect) $
      checkPretty ("bwd_expect") bwd_expect (snd in_s)
   unless (null fwd_expect) do
      when debug.logging $ logAs ("fwd ⚬ bwd") (prettyP out0')
      checkPretty ("fwd_expect") fwd_expect out0'

   recordGraphSize g

   in0 <- graphBenchmark benchNames.bwd $ \_ -> pure (evalG.bwd out0)
   checkEqual "Graph bwd" "Trace bwd" (snd in0) (snd in_e)
   -- Graph-bwd over-approximates environment slice compared to trace-bwd, because of sharing; see #896.
   -- I think don't think this affects round-tripping behaviour unless computation outputs a closure.
   out1 <- graphBenchmark benchNames.fwd $ \_ -> pure (evalG.fwd in0)
   checkEqual ("G-" <> benchNames.fwd) ("T-" <> benchNames.fwd) out1 out0'

   -- Already testing extensional equivalence above, but specifically test this case too.
   let out_top' = evalG.fwd in_top
   when testing.fwdPreservesTop $
      PrettyShow out_top' `shouldSatisfy "graph fwd preserves ⊤"` (unwrap >>> (_ == out_top))

   let GC evalG_dual = dual (GC evalG)
   in1 <- graphBenchmark benchNames.bwdDlFwdOp $ \_ -> pure (evalG_op.fwd out0)
   in2 <- graphBenchmark benchNames.bwdDlCmp $ \_ -> pure (evalG_dual.fwd out0)
   when testing.bwdDuals $
      -- should check environments too but currently requires more general checkEqual
      checkEqual benchNames.bwdDlFwdOp benchNames.bwdDlCmp (snd in1) (snd in2)
   void $ graphBenchmark benchNames.bwdAll $ \_ -> pure (evalG.bwd (topOf vα))

   out2 <- graphBenchmark benchNames.fwdDlBwdOp $ \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.fwdDlCmp $ \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      checkEqual benchNames.fwdDlBwdOp benchNames.fwdDlCmp out2 out3

   let GC evalG_dual_op = dual (GC evalG_op)
   out4 <- benchmark benchNames.naiveFwd $ \_ -> pure (evalG_dual_op.fwd in0)
   when testing.naiveFwd $
      checkEqual "Naive fwd" "Direct fwd" out4 out1

checkEqual
   :: forall m f a
    . Apply f
   => Eq (f a)
   => Pretty (f a)
   => MeetSemilattice a
   => Neg a
   => BotOf (f a) (f a)
   => MonadError Error m
   => String
   -> String
   -> f a
   -> f a
   -> m Unit
checkEqual method1 method2 x y = do
   check (spyWhen tracing.checkEqual (method1 <> " minus " <> method2) prettyP (x `lift2 (-)` y) == botOf x) (method1 <> " <= " <> method2)
   check (spyWhen tracing.checkEqual (method2 <> " minus " <> method1) prettyP (y `lift2 (-)` x) == botOf x) (method2 <> " <= " <> method1)

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)

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
