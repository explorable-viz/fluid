module Test.Util where

import Prelude hiding ((-), absurd)

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (fst, snd)
import Desug (Desugaring, desugGC)
import Effect.Class.Console (log)
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
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Util.Debug (testing, tracing)
import Util (type (×), AffError, EffectError, Thunk, Endo, check, checkSatisfies, debug, spyWhen, throw, (×))
import Val (class Ann, Val)

type Selector f = Endo (f 𝔹) -- modifies selection state

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

graphBenchmark :: forall m a. MonadWriter BenchRow m => String -> Thunk (m a) -> EffectError m a
graphBenchmark name = benchmark ("G" <> "-" <> name)

benchNames
   :: { eval :: String
      , bwd :: String
      , demBy :: String
      , fwd :: String
      , demBy_G_direct :: String
      , demBy_G_suff_dual :: String
      }

benchNames =
   { eval: "Eval" -- Neeeded
   , bwd: "Demands" -- Needed
   , demBy: "DemBy" -- Needed
   , fwd: "Suffices" -- Unsure
   , demBy_G_direct: "DemBy-Dir" -- Needed
   , demBy_G_suff_dual: "DemBy-Suff" -- Needed
   }

testProperties :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig -> SelectionSpec -> AffError m Unit
testProperties s gconfig { δv, bwd_expect, fwd_expect } = do
   let γ = erase gconfig.γ
   { gc: GC desug, e } <- desugGC s
   traced@{ gc: GC evalT, v } <- traceBenchmark benchNames.eval \_ ->
      traceGC γ e
   { gc: GC evalG, gc_op: GC evalG_op, g, vα: _ } <- graphBenchmark benchNames.eval \_ ->
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

   let (GC dualed) = (dual traced.gc)
   out0'' <- do
      let in0'' = desug'.fwd in_s
      traceBenchmark benchNames.demBy \_ -> pure (dualed.bwd in0'')
   unwrap >>> (_ >= out0'') # checkSatisfies "Force evaluation of DemBy" (PrettyShow out0'')

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
   -- Graph-bwd over-approximates environment slice compared to trace-bwd, because of sharing; see #896.
   -- I think don't think this affects round-tripping behaviour unless computation outputs a closure.
   checkEq "Graph bwd" "Trace bwd" (snd in0) (snd in_e)
   out1 <- graphBenchmark benchNames.fwd \_ -> pure (evalG.fwd in0)
   checkEq ("G-" <> benchNames.fwd) ("T-" <> benchNames.fwd) out1 out0'

   -- Already testing extensional equivalence above, but specifically test this too.
   let out_top' = evalG.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == out_top) # checkSatisfies "graph fwd preserves ⊤" (PrettyShow out_top')

   let GC evalG_dual = dual (GC evalG)

   out2 <- graphBenchmark benchNames.demBy_G_direct \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.demBy_G_suff_dual \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      checkEq benchNames.demBy_G_direct benchNames.demBy_G_suff_dual out2 out3

checkEq :: forall m a. BotOf a a => Neg a => MeetSemilattice a => Eq a => Pretty a => MonadError Error m => String -> String -> a -> a -> m Unit
checkEq op1 op2 x y = do
   let x_minus_y = x - y
   let y_minus_x = y - x
   check (x_minus_y == botOf x) (op1 <> " but not " <> op2 <> ":\n" <> prettyP x_minus_y)
   check (y_minus_x == botOf x) (op2 <> " but not " <> op1 <> ":\n" <> prettyP y_minus_x)

testPretty :: forall m a. Ann a => SE.Expr a -> AffError m Unit
testPretty s = do
   s' <- parse (prettyP s) program
   unless (eq (erase s) (erase s')) $
      throw ("parse/prettyP round trip:\nOriginal\n" <> show (erase s) <> "\nNew\n" <> show (erase s'))

checkPretty :: forall a m. Pretty a => String -> String -> a -> EffectError m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x) $
      throw (msg <> ":\nExpected\n" <> expect <> "\nReceived\n" <> prettyP x)
