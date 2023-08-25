module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Set (Set) as S
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
import Expr (Expr) as E
import Graph (Vertex, sinks, sources, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice) as G
import Graph.Slice (selectVertices, select𝔹s)
import Lattice (𝔹, bot, botOf, erase)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openDefaultImports, parse)
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Set (subset)
import Test.Spec (SpecT, before, beforeAll, beforeWith, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Mocha (runMocha)
import Util (MayFailT, type (×), (×), successful)
import Val (Env, Val(..), class Ann, (<+>))

-- Don't enforce fwd_expect values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

type Test a = SpecT Aff Unit Effect a
type TestWith g a = SpecT Aff g Effect a

run :: forall a. Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

checkPretty :: forall a m. MonadThrow Error m => Pretty a => String -> String -> a -> m Unit
checkPretty msg expect x =
   unless (expect `eq` prettyP x)
      $ fail (msg <> "\nExpected: \n" <> expect <> "\nGotten:" <> prettyP x)

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg

type TestConfig =
   { δv :: Selector Val
   , fwd_expect :: String
   , bwd_expect :: String
   }

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
testWithSetup :: SE.Expr Unit -> GraphConfig (GraphImpl S.Set) -> TestConfig -> Aff Unit
testWithSetup s gconfig tconfig =
   runExceptT
      ( do
           -- test parsing
           testParse s
           -- test trace-based
           let s𝔹 × γ𝔹 = (botOf s) × (botOf <$> gconfig.γ)
           v𝔹 × e𝔹 <- testTrace s𝔹 γ𝔹 tconfig
           -- test graph-based
           testGraph v𝔹 e𝔹 gconfig tconfig
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

testTrace :: SE.Expr 𝔹 -> Env 𝔹 -> TestConfig -> MayFailT Aff (Val 𝔹 × E.Expr 𝔹)
testTrace s γ { δv, bwd_expect, fwd_expect } = do
   -- | Eval
   e <- desug s
   t × v <- eval γ e bot
   -- | Backward
   let
      v' = δv v
      { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v' t
      s' = desugBwd e' (erase s)
   -- | Forward (round-tripping)
   _ × v'' <- desug s' >>= flip (eval γ') top

   lift $ do
      -- | Check backward selections
      unless (null bwd_expect) do
         checkPretty "Source selection" bwd_expect s'
      -- | Check round-trip selections
      unless (isGraphical v') do
         checkPretty "Value" fwd_expect v''
   pure (v' × e')

testGraph :: Val 𝔹 -> E.Expr 𝔹 -> GraphConfig (GraphImpl S.Set) -> TestConfig -> MayFailT Aff Unit
testGraph v𝔹 e𝔹 gconf { fwd_expect } = do
   -- | Eval
   (g × _) × (eα × vα) <- evalWithConfig gconf e𝔹 >>= except
   -- | Backward
   let
      αs_out = selectVertices vα v𝔹
      gbwd = G.bwdSlice αs_out g
      αs_in = sinks gbwd
      e𝔹' = select𝔹s eα αs_in
   log ("Selections on outputs: \n" <> prettyP αs_out <> "\n")
   log ("Backward-sliced graph: \n" <> prettyP gbwd <> "\n")
   -- | Forward (round-tripping)
   let
      gfwd = G.fwdSlice αs_in g
      v𝔹' = select𝔹s vα (vertices gfwd)
   log ("Selections on inputs: \n" <> prettyP αs_in <> "\n")
   log ("Forward-sliced graph: \n" <> prettyP gfwd <> "\n")

   lift $ do
      -- | Check graph/trace-based slicing procedures agree on expression
      checkPretty "Graph-based backward slicing" (prettyP e𝔹) e𝔹'
      -- | Check graph/trace-based slicing procedures agree on round-tripped value.
      unless (isGraphical v𝔹') do
         checkPretty "Graph-based round-tripping" fwd_expect v𝔹'
      sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
         (flip subset (sources gfwd))

withDefaultImports ∷ TestWith (GraphConfig (GraphImpl S.Set)) Unit -> Test Unit
withDefaultImports = beforeAll openDefaultImports

withDataset :: File -> TestWith (GraphConfig (GraphImpl S.Set)) Unit -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
withDataset dataset =
   beforeWith (openDatasetAs dataset "data" >=> (\({ g, n, γ } × xv) -> pure { g, n, γ: γ <+> xv }))

testMany :: Array (File × String) → Test Unit
testMany fxs = withDefaultImports $ traverse_ test fxs
   where
   test (file × fwd_expect) = beforeWith ((_ <$> open file) <<< (×)) $
      it (show file) (\(gconfig × s) -> testWithSetup s gconfig { δv: identity, fwd_expect, bwd_expect: mempty })

testBwdMany :: Array (File × File × Selector Val × String) → Test Unit
testBwdMany fxs = withDefaultImports $ traverse_ testBwd fxs
   where
   testBwd (file × file_expect × δv × fwd_expect) =
      beforeWith ((_ <$> open (folder <> file)) <<< (×)) $
         it (show $ folder <> file)
            ( \(gconfig × s) -> do
                 bwd_expect <- loadFile (Folder "fluid/example") (folder <> file_expect)
                 testWithSetup s gconfig { δv, fwd_expect, bwd_expect }
            )
   folder = File "slicing/"

testWithDatasetMany :: Array (File × File) -> Test Unit
testWithDatasetMany fxs = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset (dataset × file) = withDataset dataset $ beforeWith ((_ <$> open file) <<< (×)) do
      it (show file) (\(gconfig × s) -> testWithSetup s gconfig { δv: identity, fwd_expect: mempty, bwd_expect: mempty })

testLinkMany :: Array (LinkFigSpec × Selector Val × String) -> Test Unit
testLinkMany fxs = traverse_ testLink fxs
   where
   testLink (spec@{ x } × δv1 × v2_expect) = before (loadLinkFig spec) $
      it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
         \{ γ0, γ, e1, e2, t1, t2, v1 } ->
            let
               { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
            in
               checkPretty "Linked output" v2_expect v2'
