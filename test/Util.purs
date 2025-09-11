module Test.Util where

import Prelude hiding (absurd, compare)

import App.Util (Selector, getPersistent, unselected)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (fst)
import Desug (desugGC)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalGraph (GraphConfig, graphEval, graphGC, toGC, withOp)
import File (class LoadFile, File, FileCxt, Folder(..), loadFile)
import GaloisConnection (GaloisConnection(..), dual)
import Lattice (class BotOf, class MeetSemilattice, class Neg, Raw, erase, topOf, 𝔹)
import Module (parseProgram', prepConfig)
import Pretty (class Pretty, PrettyShow(..), compare, prettyP) as Old
import SExpr (Expr) as SE
import Temp.Pretty (prettyPy)
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Util.Debug (testing, tracing)
import Util (type (×), AffError, EffectError, Endo, Thunk, check, checkSatisfies, log', spyWhen, throw, withMsg, (×))
import Val (class Ann, Env, EnvExpr(..), Val)

type TestSuite m = Array (String × m Unit)

type SelectionSpec =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

fluidSrcPaths :: Array Folder
fluidSrcPaths = [ Folder "fluid", Folder "test/fluid" ]

test ∷ forall m. MonadReader FileCxt m => LoadFile m => File -> Raw Env -> SelectionSpec -> Int × Boolean -> AffError m BenchRow
test file primitives spec (n × _) = do
   fluidSrc <- loadFile fluidSrcPaths file
   log' ("**** prepConfig")
   { s, gconfig } <- prepConfig primitives fluidSrc
   testPretty s
   _ × res <- runWriterT (replicateM n (testProperties s gconfig spec))
   pure $ res `divRow` n

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

testProperties
   :: forall m
    . MonadReader FileCxt m
   => LoadFile m
   => MonadWriter BenchRow m
   => Raw SE.Expr
   -> GraphConfig
   -> SelectionSpec
   -> AffError m Unit
testProperties s gconfig { δv, bwd_expect, fwd_expect } = do
   { gc: GC desug, e } <- desugGC s

   graphed@{ g, outα } <- graphBenchmark benchNames.eval \_ ->
      graphEval gconfig e
   let GC evalG = graphGC graphed # toGC

   let v = map (const top) outα :: Val 𝔹
   let out0 = fst (δv (const unselected <$> v)) <#> getPersistent

   in0@(EnvExpr in_γ in_e) <- do
      let report = spyWhen tracing.bwdSelection "Selection for bwd" Old.prettyP
      graphBenchmark benchNames.bwd \_ -> pure (evalG.bwd (report out0))

   let in_s = desug.bwd in_e
   out1 <- do
      let in_e' = desug.fwd in_s
      unwrap >>> (_ >= in_e) # checkSatisfies "fwd ⚬ bwd round-trip (desugar)" (Old.PrettyShow in_e')
      graphBenchmark benchNames.fwd \_ -> pure (evalG.fwd (EnvExpr in_γ in_e'))
   unwrap >>> (_ >= out0) # checkSatisfies "fwd ⚬ bwd round-trip (eval)" (Old.PrettyShow out1)

   let in_top = EnvExpr (topOf in_γ) (topOf in_e)

   -- empty string somewhat hacky encoding for "don't care"
   unless (null bwd_expect) $
      checkPretty ("bwd_expect") bwd_expect in_s
   unless (null fwd_expect) do
      let report = spyWhen tracing.fwdAfterBwd "fwd ⚬ bwd" Old.prettyP
      checkPretty ("fwd_expect") fwd_expect (report out1)

   recordGraphSize g

   let out_top = evalG.fwd in_top
   when testing.fwdPreservesTop $
      unwrap >>> (_ == topOf v) # checkSatisfies "graph fwd preserves ⊤" (Old.PrettyShow out_top)

   let GC evalG_dual = dual (GC evalG)
   let GC evalG_op = withOp graphed # graphGC # toGC

   out2 <- graphBenchmark benchNames.demBy_G_direct \_ -> pure (evalG_op.bwd in0)
   out3 <- graphBenchmark benchNames.demBy_G_suff_dual \_ -> pure (evalG_dual.bwd in0)
   when testing.fwdDuals $
      checkEq benchNames.demBy_G_direct benchNames.demBy_G_suff_dual out2 out3

checkEq
   :: forall m a
    . BotOf a a
   => Neg a
   => MeetSemilattice a
   => Eq a
   => Old.Pretty a
   => MonadError Error m
   => String
   -> String
   -> a
   -> a
   -> m Unit
checkEq op1 op2 x y = do
   let left × right = Old.compare op1 op2 x y
   check (left == "") left
   check (right == "") right

testPretty :: forall m a. Ann a => Show a => SE.Expr a -> AffError m Unit
testPretty s = do
   log' ("**** prettyPy")
   log' (prettyPy s)
   s' × _ <- withMsg "testPretty" $ parseProgram' (prettyPy s)
   unless (eq (erase s) (erase s')) $
      throw ("parse/prettyPy round trip:\nOriginal\n" <> prettyPy (erase s) <> "\nNew\n" <> prettyPy (erase s'))

checkPretty :: forall a m. Old.Pretty a => String -> String -> a -> EffectError m Unit
checkPretty msg expect x =
   unless (expect `eq` Old.prettyP x) $
      throw (msg <> ":\nExpected\n" <> expect <> "\nReceived\n" <> Old.prettyP x)

testOutcome :: Boolean -> Endo String
testOutcome b s = "\x1b[" <> (if b then "32" else "31") <> "m " <> (if b then "✔" else "✖") <> "\x1b[0m " <> s

testCondition :: forall m. MonadThrow Error m => MonadEffect m => String -> Boolean -> String -> m Unit
testCondition testName b msg = do
   log (testOutcome b msg')
   when (not b) $
      throw "Test failed" -- could improve this to accumulate test failures rather than "failing fast"
   where
   msg' = testName <> ": " <> msg
