module Test.Util
   ( TestBwdSpec
   , TestConfig
   , TestSpec
   , TestWithDatasetSpec
   , checkPretty
   , isGraphical
   , shouldSatisfy
   , testParse
   , testTrace
   , testWithSetup
   ) where

import Prelude hiding (absurd)

import App.Util (Selector)
import Benchmark.Util (BenchRow(..), GraphRow, TraceRow, bench)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
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
import Lattice (bot, botOf, erase, Raw)
import Module (parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec.Assertions (fail)
import Util (MayFailT, (×), error, successful)
import Val (Val(..), class Ann)

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
-- testWithSetup :: Boolean -> SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff BenchRow
testWithSetup ∷ String -> Boolean → SE.Expr Unit → GraphConfig GraphImpl → TestConfig → Aff BenchRow
testWithSetup _name is_bench s gconfig tconfig = do
   e <- runExceptT $ do
      unless is_bench (testParse s)
      trRow <- testTrace is_bench s gconfig tconfig
      grRow <- testGraph is_bench s gconfig tconfig
      pure (BenchRow trRow grRow)
   case e of
      Left msg -> error msg
      Right x -> pure x

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src) \_ ->
      unless (eq (erase s) (erase s')) do
         log ("SRC\n" <> show (erase s))
         log ("NEW\n" <> show (erase s'))
         (lift $ fail "not equal") :: MayFailT Aff Unit

testTrace :: Boolean -> Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff TraceRow
testTrace is_bench s { γα } { δv, bwd_expect, fwd_expect } = do
   let s𝔹 × γ𝔹 = botOf s × (botOf <$> γα)
   -- | Eval
   e𝔹 <- desug s𝔹
   (t × v𝔹) × tEval <- bench $ eval γ𝔹 e𝔹 bot
   -- | Backward
   (v𝔹' × γ𝔹' × e𝔹') × tBwd <- bench $ do
      let
         v𝔹' = δv v𝔹
         { γ: γ𝔹', e: e𝔹' } = evalBwd (erase <$> γ𝔹) (erase e𝔹) v𝔹' t
      pure (v𝔹' × γ𝔹' × e𝔹')
   let
      s𝔹' = desugBwd e𝔹' s
   -- | Forward (round-tripping)
   e𝔹'' <- desug s𝔹'
   (_ × v𝔹'') × tFwd <- bench $ eval γ𝔹' e𝔹'' top

   unless is_bench $ lift do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Trace-based source selection" bwd_expect s𝔹'
      -- | Check round-trip selections
      unless (isGraphical v𝔹') do
         checkPretty "Trace-based value" fwd_expect v𝔹''
   pure { tEval, tBwd, tFwd }

testGraph :: Boolean -> Raw SE.Expr -> GraphConfig GraphImpl -> TestConfig -> MayFailT Aff GraphRow
testGraph is_bench s gconf { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   ((g × _) × (eα × vα)) × tEval <- bench $ evalWithConfig gconf e >>= except
   -- | Backward
   (gbwd × αs_in × e𝔹) × tBwd <- bench $ do
      let
         αs_out = selectαs (δv (botOf vα)) vα
         gbwd = G.bwdSlice αs_out g
         αs_in = sinks gbwd
         e𝔹 = select𝔹s eα αs_in
      pure (gbwd × αs_in × e𝔹)
   let
      s𝔹 = desugBwd e𝔹 (erase s)
   -- | Forward (round-tripping)
   (gfwd × v𝔹) × tFwd <- bench $ do
      let
         gfwd = G.fwdSlice αs_in g
         v𝔹 = select𝔹s vα (vertices gfwd)
      pure (gfwd × v𝔹)
   -- | Forward (round-tripping) using De Morgan dual
   (_ × v𝔹') × tFwdDemorgan <- bench $ do
      let
         gfwd' = G.fwdSliceDeMorgan αs_in g
         v𝔹' = select𝔹s vα (vertices gfwd') <#> not
      pure (gfwd' × v𝔹')

   unless is_bench $ lift do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
         checkPretty "Graph-based value (De Morgan)" fwd_expect v𝔹'
      sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset (sources gfwd))

   pure { tEval, tBwd, tFwd, tFwdDemorgan }

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

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

checkPretty :: forall a m. MonadThrow Error m => Pretty a => String -> String -> a -> m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x)
      $ fail (msg <> "\nExpected:\n" <> expect <> "\nGotten:\n" <> prettyP x)

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail (show v <> " doesn't satisfy predicate: " <> msg)
