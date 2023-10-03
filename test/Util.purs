module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec)
import App.Util (Selector)
import Benchmark.Util (BenchRow(..), GraphRow, TraceRow, preciseTime, tdiff)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (elem)
import Data.List.Lazy (List, length)
import Data.Set (subset)
import Data.String (null)
import DataType (dataTypeFor, typeName)
import Desug (desugGC)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import Graph (selectαs, select𝔹s, sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDual, fwdSliceDual, fwdSliceDeMorgan) as G
import GaloisConnection (GaloisConnection(..))
import Heterogeneous.Mapping (hmap)
import Lattice (Raw, botOf, erase)
import Module (parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (MayFailT, successful, (×))
import Val (class Ann, ProgCxt(..), Val(..))

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

logging :: Boolean
logging = false

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
testWithSetup ∷ String -> SE.Expr Unit → GraphConfig GraphImpl → TestConfig → Aff BenchRow
testWithSetup _name s gconfig tconfig =
   liftEither =<<
      ( runExceptT do
           testParse s
           trRow <- testTrace s gconfig tconfig
           grRow <- testGraph s gconfig tconfig
           pure $ BenchRow trRow grRow
      )

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   unless (eq (erase s) (erase s')) do
      log ("SRC\n" <> show (erase s))
      log ("NEW\n" <> show (erase s'))
      lift $ fail "not equal"

testTrace :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff TraceRow
testTrace s { progCxt: ProgCxt { γ } } { δv, bwd_expect, fwd_expect } = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   GC desug <- desugGC s
   GC desug𝔹 <- desugGC s

   -- | Eval
   let e = desug.fwd s
   t_eval1 <- preciseTime
   { gc: GC eval, v } <- traceGC (erase <$> γ) e
   t_eval2 <- preciseTime

   -- | Backward
   t_bwd1 <- preciseTime
   let γ𝔹 × e𝔹 × _ = eval.bwd (δv (botOf v))
   t_bwd2 <- preciseTime
   let s𝔹 = desug𝔹.bwd e𝔹

   -- | Forward (round-tripping)
   let e𝔹' = desug𝔹.fwd s𝔹
   t_fwd1 <- preciseTime
   let v𝔹 = eval.fwd (γ𝔹 × e𝔹' × top)
   t_fwd2 <- preciseTime

   lift do
      -- | Check backward selections
      unless (null bwd_expect) $
         checkPretty "Trace-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v) do
         when logging $ log (prettyP v𝔹)
         checkPretty "Trace-based value" fwd_expect v𝔹

   pure { tEval: tdiff t_eval1 t_eval2, tBwd: tdiff t_bwd1 t_bwd2, tFwd: tdiff t_fwd1 t_fwd2 }

testGraph :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff GraphRow
testGraph s gconfig { δv, bwd_expect, fwd_expect } = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   GC desug <- desugGC s
   GC desug𝔹 <- desugGC s

   -- | Eval
   let e = desug.fwd s
   t_eval1 <- preciseTime
   { gc: GC eval, eα, g, vα } <- graphGC gconfig e
   t_eval2 <- preciseTime

   -- | Backward
   t_bwd1 <- preciseTime
   let
      αs_out = selectαs (δv (botOf vα)) vα
      αs_in = eval.bwd αs_out
      e𝔹 = select𝔹s eα αs_in
   t_bwd2 <- preciseTime
   let s𝔹 = desug𝔹.bwd e𝔹

   -- | De Morgan dual of backward
   t_bwdDual1 <- preciseTime
   let
      αs_out_dual = selectαs (δv (botOf vα)) vα
      gbwd_dual = G.bwdSliceDual αs_out_dual g
      αs_in_dual = sinks gbwd_dual
      e𝔹_dual = select𝔹s eα αs_in_dual
   t_bwdDual2 <- preciseTime

   -- | Backward (all outputs selected)
   t_bwdAll1 <- preciseTime
   let
      e𝔹_all = select𝔹s eα $ eval.bwd (vertices vα)
   t_bwdAll2 <- preciseTime

   -- | Forward (round-tripping)
   t_fwd1 <- preciseTime
   let
      αs_out' = eval.fwd αs_in
      v𝔹 = select𝔹s vα αs_out'
   t_fwd2 <- preciseTime

   -- | De Morgan dual of forward
   t_fwdDual1 <- preciseTime
   let
      gfwd_dual = G.fwdSliceDual αs_in g
      v𝔹_dual = select𝔹s vα (vertices gfwd_dual)
   t_fwdDual2 <- preciseTime

   -- | Forward (round-tripping) using De Morgan dual
   t_fwdAsDeMorgan1 <- preciseTime
   let
      gfwd_demorgan = G.fwdSliceDeMorgan αs_in g
      v𝔹_demorgan = select𝔹s vα (vertices gfwd_demorgan) <#> not
   t_fwdAsDeMorgan2 <- preciseTime

   lift do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
         checkPretty "Graph-based value (De Morgan)" fwd_expect v𝔹_demorgan
      αs_out `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset αs_out')
      -- | To avoid unused variables when benchmarking
      when logging do
         log (prettyP e𝔹_dual)
         log (prettyP e𝔹_all)
         log (prettyP v𝔹_dual)

   pure { tEval: tdiff t_eval1 t_eval2, tBwd: tdiff t_bwd1 t_bwd2, tBwdDual: tdiff t_bwdDual1 t_bwdDual2, tBwdAll: tdiff t_bwdAll1 t_bwdAll2, tFwd: tdiff t_fwd1 t_fwd2, tFwdDual: tdiff t_fwdDual1 t_fwdDual2, tFwdAsDemorgan: tdiff t_fwdAsDeMorgan1 t_fwdAsDeMorgan2 }

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

averageRows :: List BenchRow -> BenchRow
averageRows rows = average $ foldl (<>) mempty rows
   where
   runs = toNumber $ length rows
   average (BenchRow tr gr) = BenchRow (hmap (_ `div` runs) tr) (hmap (_ `div` runs) gr)
