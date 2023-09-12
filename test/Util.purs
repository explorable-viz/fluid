module Test.Util
   ( Test
   , TestConfig
   , TestWith
   , BenchRow
   , run
   , checkPretty
   , isGraphical
   , shouldSatisfy
   , testBwdMany
   , testLinkMany
   , testMany
   , testParse
   , testTrace
   , testWithDatasetMany
   , testWithSetup
   , withDataset
   , withDefaultImports
   ) where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Benchmark.Util (bench)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold, intersperse)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Set (subset)
import Data.String (null)
import Data.Traversable (traverse_)
import DataType (dataTypeFor, typeName)
import Debug (trace)
import Desugarable (desug, desugBwd)
import Effect (Effect)
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
import Lattice (Raw, bot, botOf, erase)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports, parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Mocha (runMocha)
import Util (MayFailT, (×), error, successful)
import Val (Val(..), class Ann, (<+>))

type Test a = SpecT Aff Unit Effect a
type TestWith i a = SpecT Aff i Effect a
type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

data BenchRow = BenchRow TraceRow GraphRow

type TraceRow =
   { tEval :: Number
   , tBwd :: Number
   , tFwd :: Number
   }

type GraphRow =
   { tEval :: Number
   , tBwd :: Number
   , tFwd :: Number
   , tFwdDemorgan :: Number
   }

instance Show BenchRow where
   show (BenchRow trRow grRow) = fold $ intersperse "\n"
      [ "Trace-based eval: " <> show trRow.tEval
      , "Trace-based bwd time: " <> show trRow.tBwd
      , "Trace-based fwd time: " <> show trRow.tFwd
      , "Graph-based eval: " <> show grRow.tEval
      , "Graph-based bwd time: " <> show grRow.tBwd
      , "Graph-based fwd time:" <> show grRow.tFwd
      , "Graph-based fwd time (De Morgan): " <> show grRow.tFwdDemorgan
      ]

run :: forall a. Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
testWithSetup ∷ Boolean → Raw SE.Expr → GraphConfig GraphImpl → TestConfig → Aff BenchRow
testWithSetup is_bench s gconfig tconfig =
   runExceptT
      do
         unless is_bench (testParse s)
         BenchRow
            <$> testTrace is_bench s gconfig tconfig
            <*> testGraph is_bench s gconfig tconfig
      >>= case _ of
         Left msg -> error msg
         Right x -> log (show x) >>= const (pure x)

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src)
      \_ ->
         unless (eq (erase s) (erase s')) do
            log ("SRC\n" <> show (erase s))
            log ("NEW\n" <> show (erase s'))
            lift $ fail "not equal"

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
      pure (gbwd × αs_in × select𝔹s eα αs_in)
   let
      s𝔹 = desugBwd e𝔹 (erase s)
   -- | Forward (round-tripping)
   (gfwd × v𝔹) × tFwd <- bench $
      let gfwd = G.fwdSlice αs_in g
      in pure (gfwd × select𝔹s vα (vertices gfwd))
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

withDefaultImports ∷ TestWith (GraphConfig GraphImpl) Unit -> Test Unit
withDefaultImports = beforeAll openDefaultImports

withDataset :: File -> TestWith (GraphConfig GraphImpl) Unit -> TestWith (GraphConfig GraphImpl) Unit
withDataset dataset =
   beforeWith (openDatasetAs dataset "data" >=> \({ g, n, γα } × xv) -> pure { g, n, γα: γα <+> xv })

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
   , δv1 :: Selector Val -- relative to bot
   , v2_expect :: String
   }

testMany :: Array TestSpec → Boolean -> Test Unit
testMany fxs is_bench = withDefaultImports $ traverse_ test fxs
   where
   test :: TestSpec -> TestWith (GraphConfig GraphImpl) Unit
   test { file, fwd_expect } =
      beforeWith ((_ <$> open (File file)) <<< (×)) $
         it (show file)
            \(gconfig × s) ->
               void $ testWithSetup is_bench s gconfig { δv: identity, fwd_expect, bwd_expect: mempty }

testBwdMany :: Array TestBwdSpec → Boolean -> Test Unit
testBwdMany fxs is_bench = withDefaultImports $ traverse_ testBwd fxs
   where
   testBwd :: TestBwdSpec -> TestWith (GraphConfig GraphImpl) Unit
   testBwd { file, file_expect, δv, fwd_expect } =
      beforeWith ((_ <$> open (folder <> File file)) <<< (×)) $
         it (show $ folder <> File file)
            \(gconfig × s) -> do
               bwd_expect <- loadFile (Folder "fluid/example") (folder <> File file_expect)
               void $ testWithSetup is_bench s gconfig { δv, fwd_expect, bwd_expect }
   folder = File "slicing/"

testWithDatasetMany :: Array TestWithDatasetSpec -> Boolean -> Test Unit
testWithDatasetMany fxs is_bench = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset :: TestWithDatasetSpec -> TestWith (GraphConfig GraphImpl) Unit
   testWithDataset { dataset, file } =
      withDataset (File dataset) $ beforeWith ((_ <$> open (File file)) <<< (×)) $
         it (show file)
            \(gconfig × s) ->
               void $ testWithSetup is_bench s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty }

testLinkMany :: Array TestLinkSpec -> Test Unit
testLinkMany fxs = traverse_ testLink fxs
   where
   testLink :: TestLinkSpec -> _
   testLink { spec, δv1, v2_expect } =
      before (loadLinkFig spec) $
         it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
            \{ γ0, γ, e1, e2, t1, t2, v1 } ->
               let
                  { v': v2' } = successful $ linkResult spec.x γ0 γ e1 e2 t1 t2 (δv1 v1)
               in
                  checkPretty "Linked output" v2_expect v2'

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
