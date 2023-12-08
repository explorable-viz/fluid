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
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import GaloisConnection (GaloisConnection(..), dual)
import Graph (selectαs, select𝔹s, sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDualAsFwdOp, fwdSliceDualAsBwdOp, fwdSliceAsDeMorgan, bwdSliceDual) as G
import Lattice (Raw, 𝔹, botOf, erase, topOf)
import Module (File, open, parse)
import Parse (program)
import Pretty (class Pretty, PrettyShow(..), prettyP)
import SExpr (Expr) as SE
import Test.Benchmark.Util (BenchRow, benchmark, divRow, recordGraphSize)
import Test.Spec.Assertions (fail)
import Util (type (×), (×), AffError, EffectError, successful)
import Val (class Ann, BaseVal(..), Val(..))

type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state

type SelectionSpec =
   { δv :: Selector Val
   , fwd_expect :: String -- prettyprinted value after bwd then fwd round-trip
   , bwd_expect :: String
   }

logging :: Boolean
logging = false

logAs :: forall m. MonadEffect m => String -> String -> m Unit
logAs tag s = log $ tag <> ": " <> s

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
      when logging $ logAs (method <> "-based fwd ⚬ bwd") (prettyP v𝔹)
      checkPretty (method <> "-based fwd_expect") fwd_expect v𝔹

testTrace :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> SelectionSpec -> AffError m Unit
testTrace s gconfig spec@{ δv } = do
   let method = "T"

   { gc: GC eval, v } <- do
      GC desug <- desugGC s
      let
         e = desug.fwd s
         γ = erase <$> gconfig.γ
      benchmark (method <> "-Eval") $ \_ -> traceGC γ e

   let v𝔹 = δv (botOf v)
   γ𝔹 × e𝔹 <- do
      unless (isGraphical v𝔹) $
         when logging (logAs "Selection for bwd" (prettyP v𝔹))
      benchmark (method <> "-Bwd") $ \_ -> pure (eval.bwd v𝔹)

   GC desug𝔹 <- desugGC s
   let s𝔹 = desug𝔹.bwd e𝔹
   v𝔹' <- do
      let e𝔹' = desug𝔹.fwd s𝔹
      PrettyShow e𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (desugar)"` (unwrap >>> (_ >= e𝔹))
      benchmark (method <> "-Fwd") $ \_ -> pure (eval.fwd (γ𝔹 × e𝔹'))
   PrettyShow v𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (eval)"` (unwrap >>> (_ >= v𝔹))

   let
      v𝔹_top = topOf v
      γ𝔹_top × e𝔹_top = eval.bwd v𝔹_top
      s𝔹_top = desug𝔹.bwd e𝔹_top
      e𝔹_top' = desug𝔹.fwd s𝔹_top
      v𝔹_top' = eval.fwd (γ𝔹_top × e𝔹_top')
   PrettyShow v𝔹_top' `shouldSatisfy "fwd ⚬ bwd round-trip (eval ⚬ desugar)"` (unwrap >>> (_ >= v𝔹_top))

   validate method spec s𝔹 v𝔹'

testGraph :: forall m. MonadWriter BenchRow m => Raw SE.Expr -> GraphConfig GraphImpl -> SelectionSpec -> Boolean -> AffError m Unit
testGraph s gconfig spec@{ δv } _ = do
   let method = "G"

   { gc: gc@(GC eval), {-γα, -} eα, g, vα } <- do
      GC desug <- desugGC s
      let e = desug.fwd s
      benchmark (method <> "-Eval") $ \_ -> graphGC gconfig e

   let v𝔹 = δv (botOf vα)
   γ𝔹 × e𝔹 <- benchmark (method <> "-Bwd") $ \_ -> pure (eval.bwd v𝔹)
   v𝔹' <- benchmark (method <> "-Fwd") $ \_ -> pure (eval.fwd (γ𝔹 × e𝔹))

   GC desug𝔹 <- desugGC s
   validate method spec (desug𝔹.bwd e𝔹) v𝔹'
   PrettyShow v𝔹' `shouldSatisfy "fwd ⚬ bwd round-trip (eval)"` (unwrap >>> (_ >= v𝔹))
   recordGraphSize g

   let αs_in = selectαs e𝔹 eα
   do
      let αs = selectαs v𝔹 vα
      g' <- benchmark (method <> "-BwdDlFwdOp") $ \_ -> pure (G.bwdSliceDualAsFwdOp αs g)
      g'' <- benchmark (method <> "-BwdDlCmp") $ \_ -> pure (G.bwdSliceDual vα αs g)
      when logging (logAs "BwdDlFwdOp/input slice" (prettyP $ select𝔹s eα (sinks g')))
      when logging (logAs "BwdDlCmp/ input slice" (prettyP $ select𝔹s eα (sinks g'') <#> not))
   do
      let v𝔹_all = select𝔹s vα (vertices vα)
      _ × e𝔹' <- benchmark (method <> "-BwdAll") $ \_ -> pure (eval.bwd v𝔹_all)
      when logging (logAs "BwdAll/input slice" (prettyP e𝔹'))

   do
      g' <- benchmark (method <> "-FwdDlBwdOp") $ \_ -> pure (G.fwdSliceDualAsBwdOp αs_in g)
      v𝔹'' <- benchmark (method <> "-FwdDlCmp") $ \_ -> pure ((unwrap (dual gc)).bwd (γ𝔹 × e𝔹))
      when logging (logAs "FwdDlBwdOp/output slice" (prettyP $ select𝔹s vα (vertices g')))
      when logging (logAs "FwdDlCmp/output slice" (prettyP v𝔹''))
   do
      g' <- benchmark "Naive-Fwd" $ \_ -> pure (G.fwdSliceAsDeMorgan αs_in g)
      when logging (logAs "FwdAsDeMorgan/output slice" (prettyP $ select𝔹s vα (vertices g') <#> not))

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Val _ (Constr c _)) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v) $
      fail (show v <> " doesn't satisfy predicate: " <> msg)
