module Test.Util
   ( Test
   , Test'
   , checkPretty
   , isGraphical
   , run
   , test
   , testBwd
   , testLink
   , testWithDataset
   , testWithSetup
   ) where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Tuple (fst, snd, uncurry)
import Data.Set (Set) as S
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
import EvalGraph (evalGraph)
import Expr (Expr) as E
import Graph (Vertex, sinks, sources)
import Graph.Slice (selectVertices, selectSourcesFrom, selectSinksFrom)
import Graph.Slice (bwdSlice, fwdSlice) as G
import Graph.GraphImpl (GraphImpl)
import Lattice (𝔹, bot, erase)
import Module
   ( File(..)
   , Folder(..)
   , loadFile
   , open
   , openDatasetAs
   , openWithDefaultImports
   , parse
   )
import Parse (program)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as SE
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Mocha (runMocha)
import Util (MayFailT, type (×), (×), successful)
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
      runExceptT (testTrace γ s >>= testGraph) >>=
         case _ of
            Left msg -> fail msg
            Right unit -> pure unit

   testTrace :: Env 𝔹 -> SE.Expr 𝔹 -> MayFailT Aff (Val 𝔹 × Env 𝔹 × E.Expr 𝔹)
   testTrace γ s = do
      e <- except $ desug s
      t × v <- except $ eval γ e bot
      let
         v' = fromMaybe identity (fst <$> v_expect_opt) v
         { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v' t
         (s' :: SE.Expr 𝔹) = desugBwd e' (erase s)
      _ × v'' <- except $ desug s' >>= flip (eval γ') top
      let src = prettyP s
      s'' <- except $ parse src program
      trace ("Non-Annotated:\n" <> src) \_ -> lift $ do
         if (not $ eq (erase s) s'') then do
            liftEffect $ do
               log ("SRC\n" <> show (erase s))
               log ("NEW\n" <> show s'')
            fail "not equal"
         else do
            unless (isGraphical v'')
               (checkPretty "Value" expected v'')
            trace ("Annotated\n" <> prettyP s') \_ -> do
               case snd <$> v_expect_opt of
                  Nothing -> pure unit
                  Just file_expect -> do
                     expect <- loadFile (Folder "fluid/example") file_expect
                     checkPretty "Source selection" expect s'
         pure (v' × γ' × e')

   testGraph :: (Val 𝔹 × Env 𝔹 × E.Expr 𝔹) -> MayFailT Aff Unit
   testGraph (v𝔹 × γ𝔹 × e𝔹) = do
      g × (γα × eα × vα) <- except $ evalGraph γ𝔹 e𝔹 :: MayFailT _ (GraphImpl S.Set × _)
      lift $ do
         unless (isGraphical v𝔹 || isJust v_expect_opt)
            (checkPretty "Value" expected (erase vα))
         unless (isNothing v_expect_opt)
            ( do
                 log ("Expr 𝔹:\n" <> prettyP e𝔹)
                 log ("Val 𝔹:\n" <> prettyP v𝔹)
                 log ("Expr Vertex:\n" <> prettyP eα)
                 log ("Val Vertex:\n" <> prettyP vα)
                 --   log ("Graph:\n" <> prettyP g)
                 unless true $
                    do
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

                       -- | Check addresses on bwd graph-sliced expression match the booleans on bwd trace-sliced expression
                       let _ × e𝔹' = selectSinksFrom (γα × eα) αs_in
                       if (not $ eq e𝔹' e𝔹) then do
                          log ("Expr 𝔹 expected: \n" <> prettyP e𝔹)
                          log ("Expr 𝔹 gotten: \n" <> prettyP e𝔹')
                          fail "not equal"
                       else pure unit

                       -- | Check addresses on fwd graph-sliced value match the booleans on fwd trace-sliced value
                       let v𝔹' = selectSourcesFrom vα (sources gfwd)
                       if (not $ eq expected (prettyP v𝔹')) then do
                          log ("Val 𝔹 expected: \n" <> expected)
                          log ("Val 𝔹 gotten: \n" <> prettyP v𝔹')
                          fail "not equal"
                       else pure unit
            )

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
