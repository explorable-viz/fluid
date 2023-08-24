module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (Set) as S
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
import Test.Spec.Assertions (fail, shouldEqual)
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
checkPretty _ expect x =
   trace (":\n") \_ ->
      prettyP x `shouldEqual` expect

-- Like version in Test.Spec.Assertions but with error message.
shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg

testParse :: forall a. Ann a => SE.Expr a -> MayFailT Aff Unit
testParse s = do
   let src = prettyP s
   s' <- parse src program
   trace ("Non-Annotated:\n" <> src) (\_ ->
      unless (eq (erase s) (erase s')) do
         log ("SRC\n" <> show (erase s))
         log ("NEW\n" <> show (erase s'))
         lift $ fail "not equal")

testTrace' :: SE.Expr 𝔹 -> Env 𝔹 -> Selector Val -> Maybe String -> String -> MayFailT Aff (Val 𝔹 × E.Expr 𝔹)
testTrace' s γ v_selector bwd_expect fwd_expect = do
   -- forward
   e <- desug s
   t × v <- eval γ e bot
   -- backward
   let v_selec = v_selector v
       { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v_selec t
       s' = desugBwd e' (erase s)
   -- round-trip
   _ × v' <- desug s' >>= flip (eval γ') top

   -- check backward selections
   trace ("Annotated\n" <> prettyP s') (\_ -> do
      case bwd_expect of
         Just src -> lift $ (checkPretty "Source selection") src s'
         Nothing -> pure unit)
   -- check round-trip selections
   unless (isGraphical v') do
      lift $ checkPretty "Value" fwd_expect v'
   pure (v' × e')

-- fwd_expect: prettyprinted value after bwd then fwd round-trip
testWithSetup :: GraphConfig (GraphImpl S.Set) -> SE.Expr Unit -> String -> Selector Val -> Maybe String -> Aff Unit
testWithSetup gconfig s fwd_expect v_selector bwd_expect =
   runExceptT (testParse s >>= \_ -> testTrace gconfig >>= testGraph gconfig) >>=
      case _ of
         Left msg -> fail msg
         Right unit -> pure unit
   where
   testTrace :: GraphConfig (GraphImpl S.Set) -> MayFailT Aff (Val 𝔹 × E.Expr 𝔹)
   testTrace { γ } = do
      let
         γ𝔹 = botOf <$> γ
         s𝔹 = botOf s
      testTrace' s𝔹 γ𝔹 v_selector bwd_expect fwd_expect

   testGraph :: GraphConfig (GraphImpl S.Set) -> Val 𝔹 × E.Expr 𝔹 -> MayFailT Aff Unit
   testGraph gconf (v𝔹 × e𝔹) = do
      (g × _) × (eα × vα) <- evalWithConfig gconf (erase e𝔹) >>= except
      lift $ do
         unless (isNothing bwd_expect) $ do
            log ("Expr 𝔹:\n" <> prettyP e𝔹)
            log ("Val 𝔹:\n" <> prettyP v𝔹)
            log ("Expr Vertex:\n" <> prettyP eα)
            log ("Val Vertex:\n" <> prettyP vα)
         -- log ("Graph sources:\n" <> prettyP (sources g))

         -- | Test backward slicing
         let (αs_out :: S.Set Vertex) = selectVertices vα v𝔹
         log ("Selections on outputs: \n" <> prettyP αs_out <> "\n")
         let gbwd = G.bwdSlice αs_out g
         log ("Backward-sliced graph: \n" <> prettyP gbwd <> "\n")

         -- | Test forward slicing (via round-tripping)
         let (αs_in :: S.Set Vertex) = sinks gbwd
         log ("Selections on inputs: \n" <> prettyP αs_in <> "\n")
         let gfwd = G.fwdSlice αs_in g
         log ("Forward-sliced graph: \n" <> prettyP gfwd <> "\n")
         sources gbwd `shouldSatisfy "fwd ⚬ bwd round-tripping property"`
            (flip subset (sources gfwd))

         do
            -- | Check graph/trace-based slicing procedures agree on expression
            let e𝔹' = select𝔹s eα αs_in
            unless (eq e𝔹 e𝔹') do
               log ("Expr 𝔹 expect: \n" <> prettyP e𝔹)
               log ("Expr 𝔹 gotten: \n" <> prettyP e𝔹')
               fail "not equal"
            -- | Check graph/trace-based slicing procedures agree on round-tripped value.
            let v𝔹' = select𝔹s vα (vertices gfwd)
            unless (isGraphical v𝔹' || eq fwd_expect (prettyP v𝔹')) do
               log ("Val 𝔹 expect: \n" <> fwd_expect)
               log ("Val 𝔹 gotten: \n" <> prettyP v𝔹')
               fail "not equal"

withDefaultImports ∷ TestWith (GraphConfig (GraphImpl S.Set)) Unit -> Test Unit
withDefaultImports = beforeAll openDefaultImports

withDataset :: File -> TestWith (GraphConfig (GraphImpl S.Set)) Unit -> TestWith (GraphConfig (GraphImpl S.Set)) Unit
withDataset dataset =
   beforeWith (openDatasetAs dataset "data" >=> (\({ g, n, γ } × xv) -> pure { g, n, γ: γ <+> xv }))

testMany :: Array (File × String) → Test Unit
testMany fxs = withDefaultImports $ traverse_ test fxs
   where
   test (file × fwd_expect) = beforeWith ((_ <$> open file) <<< (×)) $
      it (show file) (\(gconfig × s) -> testWithSetup gconfig s fwd_expect identity Nothing)

testBwdMany :: Array (File × File × Selector Val × String) → Test Unit
testBwdMany fxs = withDefaultImports $ traverse_ testBwd fxs
   where
   testBwd (file × file_expect × δv × fwd_expect) =
      beforeWith ((_ <$> open (folder <> file)) <<< (×)) $
         it (show $ folder <> file)
            (\(gconfig × s) -> do
               bwd_expect <- loadFile (Folder "fluid/example") (folder <> file_expect)
               testWithSetup gconfig s fwd_expect δv (Just bwd_expect))
   folder = File "slicing/"

testWithDatasetMany :: Array (File × File) -> Test Unit
testWithDatasetMany fxs = withDefaultImports $ traverse_ testWithDataset fxs
   where
   testWithDataset (dataset × file) = withDataset dataset $ beforeWith ((_ <$> open file) <<< (×)) do
      it (show file) (\(gconfig × s) -> testWithSetup gconfig s "" identity Nothing)

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
