module Test.Util where

import Prelude hiding ((-), absurd)

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (fst, snd)
import Dict as D
import Desug (Desugaring, desugGC)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import GaloisConnection (GaloisConnection(..), (***), dual)
import Lattice (Raw, 𝔹, botOf, erase, topOf, (-))
import Module (File, initialConfig, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import ProgCxt (ProgCxt)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Util.Debug (testing, tracing)
import Util (type (×), AffError, EffectError, Thunk, check, checkSatisfies, debug, spyWhen, (×), throw)
import Val (class Ann, Val, Env)

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
   when debug.logging $ log ("**** initialConfig")
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
   { gc: GC evalT, v } <- traceBenchmark benchNames.eval \_ ->
      traceGC γ e
   { gc: GC evalG, gc_op: GC evalG_op, g, vα } <- graphBenchmark benchNames.eval \_ ->
      graphGC gconfig e

   let out0 = δv (botOf v)
   in_e <- do
      let report = spyWhen tracing.bwdSelection "Selection for bwd" prettyP
      traceBenchmark benchNames.bwd \_ -> pure (evalT.bwd (report out0))

   let GC desug' = identity *** (GC desug)
   let in_s = desug'.bwd in_e
   out0' <- do
      let in0' = desug'.fwd in_s
      unwrap >>> (_ >= in_e) # checkSatisfies "fwd ⚬ bwd round-trip (desugar)" (PrettyShow in0')
      traceBenchmark benchNames.fwd \_ -> pure (evalT.fwd in0')
   unwrap >>> (_ >= out0) # checkSatisfies "fwd ⚬ bwd round-trip (eval)" (PrettyShow out0')

   let in_top = topOf (fst in_e) × topOf (snd in_e) -- doesn't lift to pairs as intended
   let out_top = evalT.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == topOf v) # checkSatisfies "trace fwd preserves ⊤" (PrettyShow out_top)

   -- empty string somewhat hacky encoding for "don't care"
   unless (null bwd_expect) $
      checkPretty ("bwd_expect") bwd_expect (snd in_s)
   unless (null fwd_expect) do
      let report = spyWhen tracing.fwdAfterBwd "fwd ⚬ bwd" prettyP
      checkPretty ("fwd_expect") fwd_expect (report out0')

   recordGraphSize g

   in0 <- graphBenchmark benchNames.bwd \_ -> pure (evalG.bwd out0)
   checkEq "Graph bwd" "Trace bwd" (snd in0) (snd in_e)
   -- Graph-bwd over-approximates environment slice compared to trace-bwd, because of sharing; see #896.
   -- I think don't think this affects round-tripping behaviour unless computation outputs a closure.
   out1 <- graphBenchmark benchNames.fwd \_ -> pure (evalG.fwd in0)
   checkEq ("G-" <> benchNames.fwd) ("T-" <> benchNames.fwd) out1 out0'

   -- Already testing extensional equivalence above, but specifically test this too.
   let out_top' = evalG.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == out_top) # checkSatisfies "graph fwd preserves ⊤" (PrettyShow out_top')

   let GC evalG_dual = dual (GC evalG)
   in1 <- graphBenchmark benchNames.bwdDlFwdOp \_ -> pure (evalG_op.fwd out0)
   in2 <- graphBenchmark benchNames.bwdDlCmp \_ -> pure (evalG_dual.fwd out0)
   when testing.bwdDuals $ do
      checkEqEnv benchNames.bwdDlFwdOp benchNames.bwdDlCmp (fst in1) (fst in2)
      checkEq benchNames.bwdDlFwdOp benchNames.bwdDlCmp (snd in1) (snd in2)
   void $ graphBenchmark benchNames.bwdAll \_ -> pure (evalG.bwd (topOf vα))

   out2 <- graphBenchmark benchNames.fwdDlBwdOp \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.fwdDlCmp \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      checkEq benchNames.fwdDlBwdOp benchNames.fwdDlCmp out2 out3

   let GC evalG_dual_op = dual (GC evalG_op)
   out4 <- benchmark benchNames.naiveFwd \_ -> pure (evalG_dual_op.fwd in0)
   when testing.naiveFwd $
      checkEq "Naive fwd" "Direct fwd" out4 out1

checkEq :: forall m f. Apply f => Eq (f 𝔹) => Pretty (f 𝔹) => MonadError Error m => String -> String -> f 𝔹 -> f 𝔹 -> m Unit
checkEq op1 op2 x y = do
   let report = flip (spyWhen tracing.checkEq) prettyP
   check (report (op1 <> " minus " <> op2) (x `lift2 (-)` y) == botOf x) (op1 <> " <= " <> op2)
   check (report (op2 <> " minus " <> op1) (y `lift2 (-)` x) == botOf x) (op2 <> " <= " <> op1)

-- TODO: subsume with above (although see #892).
checkEqEnv :: forall m. MonadError Error m => String -> String -> Env 𝔹 -> Env 𝔹 -> m Unit
checkEqEnv op1 op2 γ γ' = do
   let report = flip (spyWhen tracing.checkEq) prettyP
   check (report (op1 <> " minus " <> op2) (γ `D.lift2 (-)` γ') == botOf γ) (op1 <> " <= " <> op2)
   check (report (op2 <> " minus " <> op1) (γ' `D.lift2 (-)` γ) == botOf γ) (op2 <> " <= " <> op1)

testPretty :: forall m a. Ann a => SE.Expr a -> AffError m Unit
testPretty s = do
   s' <- parse (prettyP s) program
   unless (eq (erase s) (erase s')) $
      throw ("parse/prettyP round trip:\nOriginal\n" <> show (erase s) <> "\nNew\n" <> show (erase s'))

checkPretty :: forall a m. Pretty a => String -> String -> a -> EffectError m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x) $
      throw (msg <> ":\nExpected\n" <> expect <> "\nReceived\n" <> prettyP x)
