module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
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
import EvalGraph (eval) as G
import Expr (Expr)
import Graph (class Graph, Vertex, WithGraph, alloc, runHeap, runGraphAccumT)
import Graph (empty) as G
import Lattice (𝔹, bot, erase)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openWithDefaultImports, parse)
import Parse (program)
import Pretty (pretty, class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Mocha (runMocha)
import Util (MayFailT, type (×), (×), successful)
import Util.Pretty (render)
import Val (Env, Val(..), (<+>))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

type Test a = SpecT Aff Unit Effect a
type Test' a = MayFailT (SpecT Aff Unit Effect) a

run :: forall a. Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

checkPretty :: forall a m. MonadThrow Error m => Pretty a => String -> String -> a -> m Unit
checkPretty _ expected x =
   trace (":\n") \_ ->
      prettyP x `shouldEqual` expected

testWithSetup :: File -> String -> Maybe (Selector × File) -> Aff (Env 𝔹 × SE.Expr 𝔹) -> Test Unit
testWithSetup (File file) expected v_expect_opt setup =
   before setup $ it file (uncurry doTest)
   where
   doTest :: Env 𝔹 -> SE.Expr 𝔹 -> Aff Unit
   doTest γ s =
      runExceptT (doTest' γ s) >>=
         case _ of
            Left msg -> fail msg
            Right unit -> pure unit

   doTest' :: Env 𝔹 -> SE.Expr 𝔹 -> MayFailT Aff Unit
   doTest' γ s = do
      e <- except $ desug s
      doGraphTest G.empty γ e
      t × v <- except $ eval γ e bot
      let
         v' = fromMaybe identity (fst <$> v_expect_opt) v
         { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v' t
         s' = desugBwd e' (erase s)
      _ × v'' <- except $ desug s' >>= flip (eval γ') top
      let src = render (pretty s)
      s'' <- except $ parse src program
      trace ("Non-Annotated:\n" <> src) \_ -> lift $
         if (not $ eq (erase s) s'') then do
            liftEffect $ do
               log ("SRC\n" <> show (erase s))
               log ("NEW\n" <> show s'')
            fail "not equal"
         else do
            unless (isGraphical v'') (checkPretty "Value" expected v'')
            trace ("Annotated\n" <> render (pretty s')) \_ ->
               case snd <$> v_expect_opt of
                  Nothing -> pure unit
                  Just file_expect -> do
                     expect <- loadFile (Folder "fluid/example") file_expect
                     checkPretty "Source selection" expect s'

doGraphTest :: forall g a. Show g => Graph g => g -> Env a -> Expr a -> MayFailT Aff Unit
doGraphTest g γ0 e0 = do
   let maybe_v × δg = runHeap $ runGraphAccumT $ runExceptT (doGraphTest' γ0 e0)
   let _ = δg g
   except maybe_v <#> const unit

doGraphTest' :: forall g a. Graph g => Env a -> Expr a -> WithGraph g (Val Vertex)
doGraphTest' γ0 e0 = do
   γ <- lift $ lift $ traverse alloc γ0
   e <- lift $ lift $ alloc e0
   n <- lift $ lift $ get
   v <- G.eval γ e S.empty :: WithGraph g _
   n' <- lift $ lift $ get
   trace (show (n' - n) <> " vertices allocated during eval.") \_ ->
      pure v

test :: File -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: File -> File -> Selector -> String -> Test Unit
testBwd file file_expect δv expected =
   testWithSetup file' expected (Just (δv × (folder <> file_expect))) (openWithDefaultImports file')
   where
   folder = File "slicing/"
   file' = folder <> file

testLink :: LinkFigSpec -> Selector -> String -> Test Unit
testLink spec@{ x } δv1 v2_expect =
   before (loadLinkFig spec) $
      it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
         \{ γ0, γ, e1, e2, t1, t2, v1 } ->
            let
               { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
            in
               checkPretty "Linked output" v2_expect v2'

testWithDataset :: File -> File -> Test Unit
testWithDataset dataset file = do
   testWithSetup file "" Nothing $ do
      γ0 × γ <- openDatasetAs dataset "data"
      ((γ0 <+> γ) × _) <$> open file
