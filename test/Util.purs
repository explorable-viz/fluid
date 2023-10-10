module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec)
import App.Util (Selector)
import Benchmark.Util (BenchRow, bench, divRow)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List (elem)
import Data.List.Lazy (replicateM)
import Data.Set (subset)
import Data.String (null)
import DataType (dataTypeFor, typeName)
import Desug (desugGC)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import Expr (ProgCxt)
import GaloisConnection (GaloisConnection(..))
import Graph (Vertex, selectαs, select𝔹s, sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDual, fwdSliceDual, fwdSliceDeMorgan) as G
import Lattice (Raw, botOf, erase)
import Module (File, initialConfig, open, parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (type (×), successful, (×))
import Val (class Ann, Env, Val(..))

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

type AffError m a = MonadAff m => MonadError Error m => m a

logging :: Boolean
logging = false

test ∷ forall m. File -> ProgCxt Unit -> TestConfig -> (Int × Boolean) -> AffError m BenchRow
test file progCxt tconfig (n × is_bench) = do
   gconfig <- initialConfig progCxt
   s <- open file
   testPretty s
   _ × row_accum <- runWriterT
      ( replicateM n $ do
           testTrace s gconfig.γ tconfig
           testGraph s gconfig tconfig is_bench
      )
   pure $ row_accum `divRow` n

testPretty :: forall m a. Ann a => SE.Expr a -> AffError m Unit
testPretty s = do
   let src = prettyP s
   s' <- parse src program
   unless (eq (erase s) (erase s')) do
      log ("SRC\n" <> show (erase s))
      log ("NEW\n" <> show (erase s'))
      fail "not equal"

testTrace :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> Env Vertex -> TestConfig -> AffError m Unit
testTrace s γ { δv, bwd_expect, fwd_expect } = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   GC desug <- desugGC s
   GC desug𝔹 <- desugGC s

   -- | Eval
   let e = desug.fwd s
   { gc: GC eval, v } <- bench "Trace-Eval" $ \_ ->
      traceGC (erase <$> γ) e

   -- | Backward
   (γ𝔹 × e𝔹 × _) <- bench "Trace-Bwd" $ \_ ->
      pure (eval.bwd (δv (botOf v)))
   let s𝔹 = desug𝔹.bwd e𝔹

   -- | Forward (round-tripping)
   let e𝔹' = desug𝔹.fwd s𝔹
   v𝔹 <- bench "Trace-Fwd" $ \_ ->
      pure (eval.fwd (γ𝔹 × e𝔹' × top))

   -- | Check backward selections
   unless (null bwd_expect) $
      checkPretty "Trace-based source selection" bwd_expect s𝔹
   -- | Check round-trip selections
   unless (isGraphical v) do
      when logging $ log (prettyP v𝔹)
      checkPretty "Trace-based value" fwd_expect v𝔹

testGraph :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> Boolean -> AffError m Unit
testGraph s gconfig { δv, bwd_expect, fwd_expect } is_bench = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   GC desug <- desugGC s
   GC desug𝔹 <- desugGC s

   -- | Eval
   let e = desug.fwd s
   { gc: GC eval, eα, g, vα } <- bench "Graph-Eval" $ \_ ->
      graphGC gconfig e

   -- | Backward
   (e𝔹 × αs_out × αs_in) <- bench "Graph-Bwd" $ \_ -> do
      let
         αs_out = selectαs (δv (botOf vα)) vα
         αs_in = eval.bwd αs_out
      pure (select𝔹s eα αs_in × αs_out × αs_in)
   let s𝔹 = desug𝔹.bwd e𝔹

   -- | Forward (round-tripping)
   (v𝔹 × αs_out') <- bench "Graph-Fwd" $ \_ -> do
      let
         αs_out' = eval.fwd αs_in
      pure (select𝔹s vα αs_out' × αs_out')

   -- | Check backward selections
   unless (null bwd_expect) $
      checkPretty "Graph-based source selection" bwd_expect s𝔹
   -- | Check round-trip selections
   unless (isGraphical v𝔹) $
      checkPretty "Graph-based value" fwd_expect v𝔹
   αs_out `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
      (flip subset αs_out')

   unless (not is_bench) do
      -- | De Morgan dual of backward
      e𝔹_dual <- bench "Graph-BwdDual" $ \_ -> do
         let
            αs_out_dual = selectαs (δv (botOf vα)) vα
            gbwd_dual = G.bwdSliceDual αs_out_dual g
            αs_in_dual = sinks gbwd_dual
         pure (select𝔹s eα αs_in_dual)

      -- | Backward (all outputs selected)
      e𝔹_all <- bench "Graph-BwdAll" $ \_ ->
         pure (select𝔹s eα $ eval.bwd (vertices vα))

      -- | De Morgan dual of forward
      v𝔹_dual <- bench "Graph-FwdDual" $ \_ -> do
         let
            gfwd_dual = G.fwdSliceDual αs_in g
         pure (select𝔹s vα (vertices gfwd_dual))

      -- | Forward (round-tripping) using De Morgan dual
      v𝔹_demorgan <- bench "Graph-FwdAsDeMorgan" $ \_ -> do
         let
            gfwd_demorgan = G.fwdSliceDeMorgan αs_in g
         pure (select𝔹s vα (vertices gfwd_demorgan) <#> not)

      -- | To avoid unused variables when benchmarking
      when logging do
         log (prettyP v𝔹_demorgan)
         log (prettyP e𝔹_dual)
         log (prettyP e𝔹_all)
         log (prettyP v𝔹_dual)

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
