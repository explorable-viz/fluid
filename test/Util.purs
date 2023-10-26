module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec)
import App.Util (Selector)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Trans (runWriterT)
import Data.List (elem)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.Set (subset)
import Data.String (null)
import DataType (dataTypeFor, typeName)
import Desug (desugGC)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import Expr (ProgCxt)
import GaloisConnection (GaloisConnection(..))
import Graph (Vertex, selectαs, select𝔹s, sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDual, fwdSliceDual, fwdSliceDeMorgan) as G
import Lattice (Raw, 𝔹, botOf, erase, topOf)
import Module (File, initialConfig, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Spec.Assertions (fail)
import Util (type (×), successful, (×))
import Val (class Ann, Env, Val(..))

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

type AffError m a = MonadAff m => MonadError Error m => m a
type EffectError m a = MonadEffect m => MonadError Error m => m a

logging :: Boolean
logging = true

logAs :: forall m. MonadEffect m => String -> String -> m Unit
logAs tag s = log $ tag <> ": " <> s

test ∷ forall m. File -> ProgCxt Unit -> TestConfig -> (Int × Boolean) -> AffError m BenchRow
test file progCxt tconfig (n × benchmarking) = do
   gconfig <- initialConfig progCxt
   s <- open file
   testPretty s
   _ × row_accum <- runWriterT
      ( replicateM n $ do
           testTrace s gconfig.γ tconfig
           testGraph s gconfig tconfig benchmarking
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

validate :: forall m. String -> TestConfig -> SE.Expr 𝔹 -> Val 𝔹 -> EffectError m Unit
validate method { bwd_expect, fwd_expect } s𝔹 v𝔹 = do
   unless (null bwd_expect) $
      checkPretty (method <> "-based bwd_expect") bwd_expect s𝔹
   unless (isGraphical v𝔹) do
      when logging $ logAs (method <> "-based fwd ⚬ bwd") (prettyP v𝔹)
      checkPretty (method <> "-based fwd_expect") fwd_expect v𝔹

testTrace :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> Env Vertex -> TestConfig -> AffError m Unit
testTrace s γα spec@{ δv } = do
   let method = "Trace"

   { gc: GC eval, v } <- do
      GC desug <- desugGC s
      let
         e = desug.fwd s
         γ = erase <$> γα
      benchmark (method <> "-Eval") $ \_ -> traceGC γ e

   γ𝔹 × e𝔹 × _ <- do
      let v𝔹 = δv (botOf v)
      unless (isGraphical v𝔹) $
         when logging (logAs "Selection for bwd" (prettyP v𝔹))
      benchmark (method <> "-Bwd") $ \_ -> pure (eval.bwd v𝔹)

   GC desug𝔹 <- desugGC s
   let s𝔹 = desug𝔹.bwd e𝔹
   v𝔹' <- do
      let e𝔹' = desug𝔹.fwd s𝔹
      benchmark (method <> "-Fwd") $ \_ -> pure (eval.fwd (γ𝔹 × e𝔹' × top))

   let
      v𝔹_top = topOf v
      γ𝔹_top × e𝔹_top × _ = eval.bwd v𝔹_top
      v𝔹_top' = eval.fwd (γ𝔹_top × e𝔹_top × top)
   PrettyShow v𝔹_top' `shouldSatisfy "fwd ⚬ bwd round-tripping property"` (unwrap >>> (_ >= v𝔹_top))

   validate method spec s𝔹 v𝔹'

testGraph :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> Boolean -> AffError m Unit
testGraph s gconfig spec@{ δv } benchmarking = do
   let method = "Graph"
   GC desug𝔹 <- desugGC s

   { gc: GC eval, eα, g, vα } <- do
      GC desug <- desugGC s
      let e = desug.fwd s
      benchmark (method <> "-Eval") $ \_ -> graphGC gconfig e

   let αs_out = selectαs (δv (botOf vα)) vα
   αs_in <- benchmark (method <> "-Bwd") $ \_ -> pure (eval.bwd αs_out)
   let e𝔹 = select𝔹s eα αs_in

   αs_out' <- benchmark (method <> "-Fwd") $ \_ -> pure (eval.fwd αs_in)
   let v𝔹' = select𝔹s vα αs_out'

   validate method spec (desug𝔹.bwd e𝔹) v𝔹'
   αs_out `shouldSatisfy "fwd ⚬ bwd round-tripping property"` (flip subset αs_out')
   recordGraphSize g

   when benchmarking do
      do
         let αs = selectαs (δv (botOf vα)) vα
         g' <- benchmark (method <> "-BwdDual") $ \_ -> pure (G.bwdSliceDual αs g)
         when logging (logAs "BwdDual/input slice" (prettyP $ select𝔹s eα (sinks g')))

      do
         let αs = vertices vα
         αs' <- benchmark (method <> "-BwdAll") $ \_ -> pure (eval.bwd αs)
         when logging (logAs "BwdAll/input slice" (prettyP $ select𝔹s eα αs'))

      do
         g' <- benchmark (method <> "-FwdDual") $ \_ -> pure (G.fwdSliceDual αs_in g)
         when logging (logAs "FwdDual/output slice" (prettyP $ select𝔹s vα (vertices g')))

      do
         g' <- benchmark (method <> "-FwdAsDeMorgan") $ \_ -> pure (G.fwdSliceDeMorgan αs_in g)
         when logging (logAs "FwdAsDeMorgan/output slice" (prettyP $ select𝔹s vα (vertices g') <#> not))

type TestSpec =
   { file :: String
   , fwd_expect :: String
   }

type TestBwdSpec =
   { file :: String
   , bwd_expect_file :: String
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

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)
