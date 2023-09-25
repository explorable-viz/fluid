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
import Debug (trace)
import Desugarable (desug, desugBwd)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalGraph (GraphConfig, evalWithConfig)
import Graph (sinks, sources, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice, fwdSliceDeMorgan) as G
import Graph.Slice (selectαs, select𝔹s)
import Heterogeneous.Mapping (hmap)
import Lattice (bot, botOf, erase, Raw)
import Module (parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (MayFailT, (×), successful)
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
testTrace s { γα: γ } { δv, bwd_expect, fwd_expect } = do
   let s𝔹 × γ𝔹 = (botOf s) × (botOf <$> γ)
   -- | Eval
   e𝔹 <- desug s𝔹
   tEval1 <- preciseTime
   t × v𝔹 <- eval γ𝔹 e𝔹 bot
   tEval2 <- preciseTime

   -- | Backward
   tBwd1 <- preciseTime
   let
      v𝔹' = δv v𝔹
      { γ: γ𝔹', e: e𝔹' } = evalBwd (erase <$> γ𝔹) (erase e𝔹) v𝔹' t
   tBwd2 <- preciseTime
   let s𝔹' = desugBwd e𝔹' s

   -- | Forward (round-tripping)
   e𝔹'' <- desug s𝔹'
   tFwd1 <- preciseTime
   _ × v𝔹'' <- eval γ𝔹' e𝔹'' top
   tFwd2 <- preciseTime

   lift do
      unless (isGraphical v𝔹') $
         log (prettyP v𝔹'')
      -- | Check backward selections
      unless (null bwd_expect) $
         checkPretty "Trace-based source selection" bwd_expect s𝔹'
      -- | Check round-trip selections
      unless (isGraphical v𝔹') $
         checkPretty "Trace-based value" fwd_expect v𝔹''

   pure { tEval: tdiff tEval1 tEval2, tBwd: tdiff tBwd1 tBwd2, tFwd: tdiff tFwd1 tFwd2 }

testGraph :: Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff GraphRow
testGraph s gconf { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   tEval1 <- preciseTime
   (g × _) × eα × vα <- evalWithConfig gconf e
   tEval2 <- preciseTime

   -- | Backward
   tBwd1 <- preciseTime
   let
      αs_out = selectαs (δv (botOf vα)) vα
      gbwd = G.bwdSlice αs_out g
      αs_in = sinks gbwd
      e𝔹 = select𝔹s eα αs_in
   tBwd2 <- preciseTime
   let
      s𝔹 = desugBwd e𝔹 (erase s)

   -- | Forward (round-tripping)
   tFwd1 <- preciseTime
   let
      gfwd = G.fwdSlice αs_in g
      v𝔹 = select𝔹s vα (vertices gfwd)
   tFwd2 <- preciseTime

   -- | Forward (round-tripping) using De Morgan dual
   tFwdDeMorgan1 <- preciseTime
   let
      gfwd' = G.fwdSliceDeMorgan αs_in g
      v𝔹' = select𝔹s vα (vertices gfwd') <#> not
   tFwdDeMorgan2 <- preciseTime

   lift do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
         checkPretty "Graph-based value (De Morgan)" fwd_expect v𝔹'
      sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset (sources gfwd))

   pure { tEval: tdiff tEval1 tEval2, tBwd: tdiff tBwd1 tBwd2, tFwd: tdiff tFwd1 tFwd2, tFwdDemorgan: tdiff tFwdDeMorgan1 tFwdDeMorgan2 }

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

   zeroRow :: BenchRow
   zeroRow = BenchRow { tEval: 0.0, tBwd: 0.0, tFwd: 0.0 } { tEval: 0.0, tBwd: 0.0, tFwd: 0.0, tFwdDemorgan: 0.0 }

   sumRow :: BenchRow -> BenchRow -> BenchRow
   sumRow (BenchRow trRow1 gRow1) (BenchRow trRow2 gRow2) =
      BenchRow
         { tEval: trRow1.tEval + trRow2.tEval
         , tBwd: trRow1.tBwd + trRow2.tBwd
         , tFwd: trRow1.tFwd + trRow2.tFwd
         }
         { tEval: gRow1.tEval + gRow2.tEval
         , tBwd: gRow1.tBwd + gRow2.tBwd
         , tFwd: gRow1.tFwd + gRow2.tFwd
         , tFwdDemorgan: gRow1.tFwdDemorgan + gRow2.tFwdDemorgan
         }

   summed = foldl sumRow zeroRow rows
   averagedTr = (\(BenchRow tr gr) -> BenchRow (hmap (\num -> num `div` runs) tr) (hmap (\num -> num `div` runs) gr)) $ summed
