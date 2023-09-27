module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec)
import App.Util (Selector)
import BoolAlg (bool)
import Benchmark.Util (BenchRow(..), GraphRow, TraceRow, zeroRow, sumRow, preciseTime, tdiff)
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
import Debug (trace)
import Desug (desugGC)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import EvalBwd (traceGC)
import EvalGraph (GraphConfig, graphGC)
import Graph (sinks, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSliceDual, fwdSliceDual, fwdSliceDeMorgan) as G
import Heterogeneous.Mapping (hmap)
import Lattice (botOf, topOf, erase, Raw)
import Module (parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (MayFailT, successful, (×))
import Val (Val(..), class Ann)

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
-- testWithSetup :: Boolean -> SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff BenchRow
testWithSetup ∷ String -> SE.Expr Unit → GraphConfig GraphImpl → TestConfig → Aff BenchRow
testWithSetup _name s gconfig tconfig =
   liftEither =<<
      ( runExceptT $ do
           testParse s
           trRow <- testTrace s gconfig tconfig
           grRow <- testGraph s gconfig tconfig
           pure $ BenchRow trRow grRow
      )

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src) \_ ->
      unless (eq (erase s) (erase s')) do
         log ("SRC\n" <> show (erase s))
         log ("NEW\n" <> show (erase s'))
         lift $ fail "not equal"

testTrace :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff TraceRow
testTrace s { γα } { δv, bwd_expect, fwd_expect } = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   gc_desug <- desugGC s
   gc_desug𝔹 <- desugGC s

   -- | Eval
   let e = gc_desug.fwd s
   t_eval1 <- preciseTime
   gc <- traceGC bool (erase <$> γα) e
   t_eval2 <- preciseTime

   -- | Backward
   t_bwd1 <- preciseTime
   let γ𝔹 × e𝔹 × _ = gc.bwd (δv (botOf gc.v))
   t_bwd2 <- preciseTime
   let s𝔹 = gc_desug𝔹.bwd e𝔹

   -- | Forward (round-tripping)
   let e𝔹' = gc_desug𝔹.fwd s𝔹
   t_fwd1 <- preciseTime
   let v𝔹 = gc.fwd (γ𝔹 × e𝔹' × top)
   t_fwd2 <- preciseTime

   lift do
      unless (isGraphical gc.v) $
         log (prettyP v𝔹)
      -- | Check backward selections
      unless (null bwd_expect) $
         checkPretty "Trace-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical gc.v) $
         checkPretty "Trace-based value" fwd_expect v𝔹

   pure { tEval: tdiff t_eval1 t_eval2, tBwd: tdiff t_bwd1 t_bwd2, tFwd: tdiff t_fwd1 t_fwd2 }

testGraph :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff GraphRow
testGraph s gconfig { δv, bwd_expect, fwd_expect } = do
   -- | Desugaring Galois connections for Unit and Boolean type selections
   gc_desug <- desugGC s
   gc_desug𝔹 <- desugGC s

   -- | Eval
   let e = gc_desug.fwd s
   t_eval1 <- preciseTime
   gc <- graphGC gconfig e
   t_eval2 <- preciseTime

   -- | Backward
   t_bwd1 <- preciseTime
   let
      αs_out = gc.runδv δv
      αs_in = gc.bwd αs_out
      e𝔹 = gc.selecte𝔹 αs_in
   t_bwd2 <- preciseTime
   let s𝔹 = gc_desug𝔹.bwd e𝔹

   -- | De Morgan dual of backward
   t_bwdDual1 <- preciseTime
   let
      αs_out_dual = gc.runδv δv
      gbwd_dual = G.bwdSliceDual αs_out_dual gc.g
      αs_in_dual = sinks gbwd_dual
      e𝔹_dual = gc.selecte𝔹 αs_in_dual
   t_bwdDual2 <- preciseTime

   -- | Backward (all outputs selected)
   t_bwdAll1 <- preciseTime
   let
      e𝔹_all = (gc.selecte𝔹 <<< gc.bwd <<< gc.runδv) topOf
   t_bwdAll2 <- preciseTime

   -- | Forward (round-tripping)
   t_fwd1 <- preciseTime
   let
      αs_out' = gc.fwd αs_in
      v𝔹 = gc.selectv𝔹 αs_out'
   t_fwd2 <- preciseTime

   -- | De Morgan dual of forward
   t_fwdDual1 <- preciseTime
   let
      gfwd_dual = G.fwdSliceDual αs_in gc.g
      v𝔹_dual = gc.selectv𝔹 (vertices gfwd_dual)
   t_fwdDual2 <- preciseTime

   -- | Forward (round-tripping) using De Morgan dual
   t_fwdAsDeMorgan1 <- preciseTime
   let
      gfwd_demorgan = G.fwdSliceDeMorgan αs_in gc.g
      v𝔹_demorgan = gc.selectv𝔹 (vertices gfwd_demorgan) <#> not
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
      unless false do
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
averageRows rows = averagedTr
   where
   runs = toNumber $ length rows

   summed = foldl sumRow zeroRow rows
   averagedTr = (\(BenchRow tr gr) -> BenchRow (hmap (\num -> num `div` runs) tr) (hmap (\num -> num `div` runs) gr)) $ summed
