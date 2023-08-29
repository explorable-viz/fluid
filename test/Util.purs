module Test.Util
   ( Test
   , TestConfig
   , TestWith
   , checkPretty
   , isGraphical
   , run
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
import Benchmark.Util (getCurr, timeDiff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.JSDate (now)
import Data.List (elem)
import Data.Set (Set) as S
import Data.String (null)
import Data.Traversable (traverse_)
import DataType (dataTypeFor, typeName)
import Debug (trace)
import Desugarable (desug, desugBwd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalGraph (GraphConfig, evalWithConfig)
import Graph (sinks, sources, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice) as G
import Graph.Slice (selectαs, select𝔹s)
import Lattice (bot, botOf, erase)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports, parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Set (subset)
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Mocha (runMocha)
import Util (MayFailT, type (×), (×), successful)
import Val (Val(..), class Ann, (<+>))

type Test a = SpecT Aff Unit Effect a
type TestWith g a = SpecT Aff g Effect a
type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

run :: forall a. Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

switchWithSetup :: Boolean -> SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff Unit
switchWithSetup isBench s gconfig tconfig =
   if isBench then benchWithSetup s gconfig tconfig
   else testWithSetup s gconfig tconfig

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
testWithSetup :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff Unit
testWithSetup s gconfig tconfig =
   runExceptT
      ( do
           testParse s
           testTrace s gconfig tconfig
           testGraph s gconfig tconfig
      ) >>=
      case _ of
         Left msg -> fail msg
         Right unit -> pure unit

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
benchWithSetup :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff Unit
benchWithSetup s gconfig tconfig =
   runExceptT
      ( do
           testParse s
           benchTrace s gconfig tconfig
           benchGraph s gconfig tconfig
      ) >>=
      case _ of
         Left msg -> fail msg
         Right unit -> pure unit

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src)
      ( \_ ->
           unless (eq (erase s) (erase s')) do
              log ("SRC\n" <> show (erase s))
              log ("NEW\n" <> show (erase s'))
              lift $ fail "not equal"
      )

testTrace :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
testTrace s { γα } { δv, bwd_expect, fwd_expect } = do
   let s𝔹 × γ𝔹 = (botOf s) × (botOf <$> γα)
   -- | Eval
   e𝔹 <- desug s𝔹
   t × v𝔹 <- eval γ𝔹 e𝔹 bot
   -- | Backward
   let
      v𝔹' = δv v𝔹
      { γ: γ𝔹', e: e𝔹' } = evalBwd (erase <$> γ𝔹) (erase e𝔹) v𝔹' t
      s𝔹' = desugBwd e𝔹' s
   -- | Forward (round-tripping)
   _ × v𝔹'' <- desug s𝔹' >>= flip (eval γ𝔹') top

   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Trace-based source selection" bwd_expect s𝔹'
      -- | Check forward (round-tripping) selections
      unless (isGraphical v𝔹') do
         checkPretty "Trace-based value" fwd_expect v𝔹''

benchTrace :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
benchTrace s { γ } { δv, bwd_expect, fwd_expect } = do
   let s𝔹 × γ𝔹 = (botOf s) × (botOf <$> γ)
   -- | Eval
   pre_desug <- getCurr
   e𝔹 <- desug s𝔹
   pre_eval <- getCurr
   t × v𝔹 <- eval γ𝔹 e𝔹 bot
   post_eval <- getCurr
   log ("Desug time: " <> show (timeDiff pre_desug pre_eval) <> "\n")
   log ("Trace-based eval: " <> show (timeDiff pre_eval post_eval) <> "\n")
   -- | Backward
   pre_slice <- getCurr
   let
      v𝔹' = δv v𝔹
      { γ: γ𝔹', e: e𝔹' } = evalBwd (erase <$> γ𝔹) (erase e𝔹) v𝔹' t
   post_slice <- getCurr
   log ("Trace-based bwd slice time: " <> show (timeDiff pre_slice post_slice) <> "\n")
   let
      s𝔹' = desugBwd e𝔹' s
   -- | Forward (round-tripping)
   e𝔹'' <- desug s𝔹'
   pre_fwd_slice <- getCurr
   _ × v𝔹'' <- eval γ𝔹' e𝔹'' top
   post_fwd_slice <- getCurr
   log ("Trace-based fwd slice time:" <> show (timeDiff pre_fwd_slice post_fwd_slice) <> "\n")
   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Trace-based source selection" bwd_expect s𝔹'
      -- | Check round-trip selections
      unless (isGraphical v𝔹') do
         checkPretty "Trace-based value" fwd_expect v𝔹''

benchTrace :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
benchTrace s { γ } { δv, bwd_expect, fwd_expect } = do
   let s𝔹 × γ𝔹 = (botOf s) × (botOf <$> γ)
   -- | Eval
   pre_desug <- getCurr
   e𝔹 <- desug s𝔹
   pre_eval <- getCurr
   t × v𝔹 <- eval γ𝔹 e𝔹 bot
   post_eval <- getCurr
   log ("Desug time: " <> show (timeDiff pre_desug pre_eval) <> "\n")
   log ("Trace-based eval: " <> show (timeDiff pre_eval post_eval) <> "\n")
   -- | Backward
   pre_slice <- getCurr
   let
      v𝔹' = δv v𝔹
      { γ: γ𝔹', e: e𝔹' } = evalBwd (erase <$> γ𝔹) (erase e𝔹) v𝔹' t
   post_slice <- getCurr
   log ("Trace-based bwd slice time: " <> show (timeDiff pre_slice post_slice) <> "\n")
   let
      s𝔹' = desugBwd e𝔹' s
   -- | Forward (round-tripping)
   e𝔹'' <- desug s𝔹'
   pre_fwd_slice <- getCurr
   _ × v𝔹'' <- eval γ𝔹' e𝔹'' top
   post_fwd_slice <- getCurr
   log ("Trace-based fwd slice time:" <> show (timeDiff pre_fwd_slice post_fwd_slice) <> "\n")
   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Trace-based source selection" bwd_expect s𝔹'
      -- | Check round-trip selections
      unless (isGraphical v𝔹') do
         checkPretty "Trace-based value" fwd_expect v𝔹''

testGraph :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
testGraph s gconf { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   (g × _) × (eα × vα) <- evalWithConfig gconf e >>= except
   -- | Backward
   let
      αs_out = selectαs (δv (botOf vα)) vα
      gbwd = G.bwdSlice αs_out g
      αs_in = sinks gbwd
      e𝔹 = select𝔹s eα αs_in
      s𝔹 = desugBwd e𝔹 (erase s)
   -- | Forward (round-tripping)
   let
      gfwd = G.fwdSlice αs_in g
      v𝔹 = select𝔹s vα (vertices gfwd)

   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check forward (round-tripping) selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
      -- | Check round-tripping property
      sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset (sources gfwd))

benchGraph :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
benchGraph s gconf { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   pre_eval <- liftEffect now
   (g × _) × (eα × vα) <- evalWithConfig gconf e >>= except
   post_eval <- liftEffect now
   log ("Graph-based eval time: " <> show (timeDiff pre_eval post_eval) <> "\n")
   -- | Backward
   pre_slice <- getCurr
   let
      αs_out = selectVertices (δv (botOf vα)) vα
      gbwd = G.bwdSlice αs_out g
      αs_in = sinks gbwd
   post_slice <- getCurr
   log ("Graph-based bwd slice time: " <> show (timeDiff pre_slice post_slice) <> "\n")
   let
      e𝔹 = select𝔹s eα αs_in
      s𝔹 = desugBwd e𝔹 (erase s)
   -- | Forward (round-tripping)
   pre_fwd_slice <- getCurr
   let
      gfwd = G.fwdSlice αs_in g
      v𝔹 = select𝔹s vα (vertices gfwd)
   post_fwd_slice <- getCurr
   log ("Graph-based fwd slice time: " <> show (timeDiff pre_fwd_slice post_fwd_slice) <> "\n")

   {- | Forward (round-tripping) using De Morgan dual
      gfwd' = G.fwdSliceDeMorgan αs_in g
      v𝔹' = select𝔹s vα (vertices gfwd') <#> not
   -}
   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Graph-based source selection" bwd_expect s𝔹
      -- | Check round-trip selections
      unless (isGraphical v𝔹) do
         checkPretty "Graph-based value" fwd_expect v𝔹
      -- checkPretty "Graph-based value (De Morgan)" fwd_expect v𝔹'
      sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset (sources gfwd))

withDefaultImports ∷ TestWith (GraphConfig (GraphImpl S.Set)) Unit -> Test Unit
withDefaultImports = beforeAll openDefaultImports

withDataset :: File -> TestWith (GraphConfig (GraphImpl S.Set)) Unit -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
withDataset dataset = beforeWith (openDatasetAs dataset "data" >=> (\({ g, n, γα } × xv) -> pure { g, n, γα: γα <+> xv }))

testMany :: Array (File × String) → Test Unit
testMany fxs = withDefaultImports $ traverse_ test fxs
   where
   test (file × fwd_expect) =
      beforeWith ((_ <$> open file) <<< (×))
         ( it (show file)
              (\(gconfig × s) -> testWithSetup s gconfig { δv: identity, fwd_expect, bwd_expect: mempty })
         )

benchMany :: Array (File × String) -> Test Unit
benchMany fxs = withDefaultImports $ traverse_ test fxs
   where
   test (file × fwd_expect) = beforeWith ((_ <$> open file) <<< (×)) $
      it (show file) (\(gconfig × s) -> benchWithSetup s gconfig { δv: identity, fwd_expect, bwd_expect: mempty })

benchMany :: Array (File × String) -> Test Unit
benchMany fxs = withDefaultImports $ traverse_ test fxs
   where
   test (file × fwd_expect) = beforeWith ((_ <$> open file) <<< (×)) $
      it (show file) (\(gconfig × s) -> benchWithSetup s gconfig { δv: identity, fwd_expect, bwd_expect: mempty })

testBwdMany :: Array (File × File × Selector Val × String) → Test Unit
testBwdMany fxs = withDefaultImports $ traverse_ testBwd fxs
   where
   folder = File "slicing/"
   testBwd (file × file_expect × δv × fwd_expect) =
      beforeWith ((_ <$> open (folder <> file)) <<< (×))
         ( it (show $ folder <> file)
              ( \(gconfig × s) -> do
                   bwd_expect <- loadFile (Folder "fluid/example") (folder <> file_expect)
                   testWithSetup s gconfig { δv, fwd_expect, bwd_expect }
              )
         )

benchBwdMany :: Array (File × File × Selector Val × String) → Test Unit
benchBwdMany fxs = withDefaultImports $ traverse_ testBwd fxs
   where
   folder = File "slicing/"
   testBwd (file × file_expect × δv × fwd_expect) =
      beforeWith ((_ <$> open (folder <> file)) <<< (×)) $
         it (show $ folder <> file)
            ( \(gconfig × s) -> do
                 bwd_expect <- loadFile (Folder "fluid/example") (folder <> file_expect)
                 benchWithSetup s gconfig { δv, fwd_expect, bwd_expect }
            )
   folder = File "slicing/"

benchBwdMany :: Array (File × File × Selector Val × String) → Test Unit
benchBwdMany fxs = withDefaultImports $ traverse_ testBwd fxs
   where
   testBwd (file × file_expect × δv × fwd_expect) =
      beforeWith ((_ <$> open (folder <> file)) <<< (×)) $
         it (show $ folder <> file)
            ( \(gconfig × s) -> do
                 bwd_expect <- loadFile (Folder "fluid/example") (folder <> file_expect)
                 benchWithSetup s gconfig { δv, fwd_expect, bwd_expect }
            )
   folder = File "slicing/"

testWithDatasetMany :: Array (File × File) -> Test Unit
testWithDatasetMany fxs = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset (dataset × file) = withDataset dataset $ beforeWith ((_ <$> open file) <<< (×)) do
      it (show file)
         (\(gconfig × s) -> testWithSetup s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty })

benchWithDatasetMany :: Array (File × File) -> Test Unit
benchWithDatasetMany fxs = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset (dataset × file) = withDataset dataset $ beforeWith ((_ <$> open file) <<< (×)) do
      it (show file) (\(gconfig × s) -> benchWithSetup s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty })

benchWithDatasetMany :: Array (File × File) -> Test Unit
benchWithDatasetMany fxs = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset (dataset × file) = withDataset dataset $ beforeWith ((_ <$> open file) <<< (×)) do
      it (show file) (\(gconfig × s) -> benchWithSetup s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty })

testLinkMany :: Array (LinkFigSpec × Selector Val × String) -> Test Unit
testLinkMany fxs = traverse_ testLink fxs
   where
   testLink (spec@{ x } × δv1 × v2_expect) =
      before (loadLinkFig spec)
         ( it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
              ( \{ γ0, γ, e1, e2, t1, t2, v1 } ->
                   let
                      { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
                   in
                      checkPretty "Linked output" v2_expect v2'
              )
         )

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
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg
