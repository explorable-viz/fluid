module Test.Util where

import Prelude hiding (absurd, compare)

import App.Util (Selector)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Desug (Desugaring, desugGC)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphEval, graphGC, withOp)
import GaloisConnection (GaloisConnection(..), dual)
import Lattice (class BotOf, class MeetSemilattice, class Neg, Raw, botOf, erase, topOf)
import Module (File, initialConfig, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), compare, prettyP)
import ProgCxt (ProgCxt)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Util.Debug (testing, tracing)
import Util (type (×), AffError, EffectError, Thunk, check, checkSatisfies, debug, spyWhen, throw, (×))
import Val (class Ann, EnvExpr(..), Val)

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
   _ × res <- runWriterT (replicateM n (testProperties s gconfig spec))
   pure $ res `divRow` n

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
   { eval: "Eval"
   , bwd: "Demands"
   , demBy: "DemBy"
   , fwd: "Suffices" -- needed?
   , demBy_G_direct: "DemBy-Dir"
   , demBy_G_suff_dual: "DemBy-Suff"
   }

testProperties :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig -> SelectionSpec -> AffError m Unit
testProperties s gconfig { δv, bwd_expect, fwd_expect } = do
   let γ = erase gconfig.γ
   { gc: GC desug, e } <- desugGC s
   traced@{ gc: GC evalT, v } <- traceBenchmark benchNames.eval \_ ->
      traceGC (EnvExpr γ e)
   graphed@{ g } <- graphBenchmark benchNames.eval \_ ->
      graphEval gconfig e

   let out0 = δv (botOf v) <#> unwrap >>> _.persistent
   EnvExpr in_γ in_e <- do
      let report = spyWhen tracing.bwdSelection "Selection for bwd" prettyP
      traceBenchmark benchNames.bwd \_ -> pure (evalT.bwd (report out0))

   let in_s = desug.bwd in_e
   out0' <- do
      let in_e' = desug.fwd in_s
      unwrap >>> (_ >= in_e) # checkSatisfies "fwd ⚬ bwd round-trip (desugar)" (PrettyShow in_e')
      traceBenchmark benchNames.fwd \_ -> pure (evalT.fwd (EnvExpr in_γ in_e'))
   unwrap >>> (_ >= out0) # checkSatisfies "fwd ⚬ bwd round-trip (eval)" (PrettyShow out0')

   let GC dualed = dual traced.gc
   out0'' <- do
      let in0'' = desug.fwd in_s
      traceBenchmark benchNames.demBy \_ -> pure (dualed.bwd (EnvExpr in_γ in0''))
   unwrap >>> (_ >= out0'') # checkSatisfies "Force evaluation of DemBy" (PrettyShow out0'')

   let in_top = EnvExpr (topOf in_γ) (topOf in_e)
   let out_top = evalT.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == topOf v) # checkSatisfies "trace fwd preserves ⊤" (PrettyShow out_top)

   -- empty string somewhat hacky encoding for "don't care"
   unless (null bwd_expect) $
      checkPretty ("bwd_expect") bwd_expect in_s
   unless (null fwd_expect) do
      let report = spyWhen tracing.fwdAfterBwd "fwd ⚬ bwd" prettyP
      checkPretty ("fwd_expect") fwd_expect (report out0')

   recordGraphSize g
   let GC evalG = graphGC graphed

   in0 <- graphBenchmark benchNames.bwd \_ -> pure (evalG.bwd out0)
   -- Graph-bwd over-approximates environment slice compared to trace-bwd, because of sharing; see #896.
   -- I think don't think this affects round-tripping behaviour unless computation outputs a closure.
   checkEq "Graph bwd" "Trace bwd" ((\(EnvExpr _ e') -> e') in0) in_e
   out1 <- graphBenchmark benchNames.fwd \_ -> pure (evalG.fwd in0)
   checkEq ("G-" <> benchNames.fwd) ("T-" <> benchNames.fwd) out1 out0'

   -- Already testing extensional equivalence above, but specifically test this too.
   let out_top' = evalG.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == out_top) # checkSatisfies "graph fwd preserves ⊤" (PrettyShow out_top')

   let GC evalG_dual = dual (GC evalG)
   let GC evalG_op = withOp graphed # graphGC

   out2 <- graphBenchmark benchNames.demBy_G_direct \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.demBy_G_suff_dual \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      checkEq benchNames.demBy_G_direct benchNames.demBy_G_suff_dual out2 out3

checkEq :: forall m a. BotOf a a => Neg a => MeetSemilattice a => Eq a => Pretty a => MonadError Error m => String -> String -> a -> a -> m Unit
checkEq op1 op2 x y = do
   let left × right = compare op1 op2 x y
   check (left == "") left
   check (right == "") right

testPretty :: forall m a. Ann a => SE.Expr a -> AffError m Unit
testPretty s = do
   s' <- parse (prettyP s) program
   unless (eq (erase s) (erase s')) $
      throw ("parse/prettyP round trip:\nOriginal\n" <> show (erase s) <> "\nNew\n" <> show (erase s'))

checkPretty :: forall a m. Pretty a => String -> String -> a -> EffectError m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x) $
      throw (msg <> ":\nExpected\n" <> expect <> "\nReceived\n" <> prettyP x)
