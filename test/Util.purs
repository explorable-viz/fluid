module Test.Util where

import Prelude hiding ((-), absurd)

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List (elem)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (snd)
import DataType (dataTypeFor, typeName)
import Desug (desugGC)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import GaloisConnection (GaloisConnection(..), (***), dual)
import Graph.GraphImpl (GraphImpl)
import Lattice (Raw, 𝔹, (-), botOf, erase, topOf)
import Module (File, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, logAs, recordGraphSize)
import Test.Spec.Assertions (fail)
import Test.Util.Debug (testing)
import Util (type (×), AffError, EffectError, Thunk, check, debug, spy, successful, (×))
import Val (class Ann, BaseVal(..), Val(..))

type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state

type SelectionSpec =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

test ∷ forall m. File -> GraphConfig GraphImpl -> SelectionSpec -> Int × Boolean -> AffError m BenchRow
test file gconfig spec (n × _) = do
   s <- open file
   testPretty s
   _ × row_accum <- runWriterT (replicateM n (testProperties s gconfig spec))
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

testProperties :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> SelectionSpec -> AffError m Unit
testProperties s gconfig spec@{ δv, bwd_expect, fwd_expect } = do
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

   let in_top = (topOf <$> gconfig.γ) × topOf e
   let out_top = evalT.fwd in_top
   when testing.fwdPreservesTop $
      PrettyShow out_top `shouldSatisfy "trace fwd preserves ⊤"` (unwrap >>> (_ == topOf v))

   unless (null bwd_expect) $
      checkPretty (traceMethod <> "-based bwd_expect") bwd_expect (snd in_s)
   unless (isGraphical out0') do
      when debug.logging $ logAs (traceMethod <> "-based fwd ⚬ bwd") (prettyP out0')
      checkPretty (traceMethod <> "-based fwd_expect") fwd_expect out0'

   recordGraphSize g

   in0 <- graphBenchmark benchNames.bwd $ \_ -> pure (evalG.bwd out0)
   check (snd in0 == snd in_e) "Graph bwd agrees with trace bwd on expression slice"
   -- Graph-bwd over-approximates environment slice compared to trace-bwd, because of sharing; see #896.
   -- I think don't think this affects round-tripping behaviour unless computation outputs a closure.
   out1 <- graphBenchmark benchNames.fwd $ \_ -> pure (evalG.fwd in0)
   check (out1 == out0') "Graph fwd agrees with trace fwd"

   -- Already testing extensional equivalence above, but specifically test this case too.
   let out_top' = evalG.fwd in_top
   when testing.fwdPreservesTop $
      PrettyShow out_top' `shouldSatisfy "graph fwd preserves ⊤"` (unwrap >>> (_ == out_top))
   validate graphMethod spec (snd in_s) out1

   let GC evalG_dual = dual (GC evalG)
   in1 <- graphBenchmark benchNames.bwdDlFwdOp $ \_ -> pure (evalG_op.fwd out0)
   in2 <- graphBenchmark benchNames.bwdDlCmp $ \_ -> pure (evalG_dual.fwd out0)
   when testing.bwdDuals $
      check (in1 == in2) "Two constructions of bwd dual agree"
   void $ graphBenchmark benchNames.bwdAll $ \_ -> pure (evalG.bwd (topOf vα))

   out2 <- graphBenchmark benchNames.fwdDlBwdOp $ \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.fwdDlCmp $ \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      check (out2 == out3) "Two constructions of fwd dual agree"

   let GC evalG_dual_op = dual (GC evalG_op)
   out4 <- benchmark benchNames.naiveFwd $ \_ -> pure (evalG_dual_op.fwd in0)
   when testing.naiveFwd $ do
      check (spy "Direct minus naive" prettyP (out1 `lift2 (-)` out4) == botOf out1) "Direct <= naive"
      check (spy "Naive minus direct" prettyP (out4 `lift2 (-)` out1) == botOf out1) "Naive <= direct"

--      check (out4 == out1) "Naive and direct fwd agree"

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Val _ (Constr c _)) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement" ]
isGraphical _ = false

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)
