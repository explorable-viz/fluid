 module Test.Util where

import Prelude hiding (absurd)
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Debug (trace)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import DataType (dataTypeFor, typeName)
import DesugarBwd (desugarBwd)
import DesugarFwd (desugarFwd)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openWithDefaultImports)
import Pretty (class Pretty, prettyP)
import SExpr (Expr) as S
import Trace (Trace)
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..), concat)

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` ["GraphicsElement", "Plot"]
isGraphical _              = false

type Test a = SpecT Aff Unit Effect a

run :: forall a . Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

desugarEval :: Env 𝔹 -> S.Expr 𝔹 -> MayFail (Trace 𝔹 × Val 𝔹)
desugarEval γ s = desugarFwd s >>= eval γ

desugarEval_bwd :: Trace 𝔹 × S.Expr 𝔹 -> Val 𝔹 -> Env 𝔹 × S.Expr 𝔹
desugarEval_bwd (t × s) v =
   let γ × e × _ = evalBwd v t in
   γ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Trace 𝔹 -> Val 𝔹
desugarEval_fwd γ s = evalFwd γ (successful (desugarFwd s)) true

checkPretty :: forall a . Pretty a => String -> String -> a -> Aff Unit
checkPretty msg expected x =
   trace (msg <> ":\n" <> prettyP x) \_ ->
      prettyP x `shouldEqual` expected

-- v_expect_opt is optional output slice + expected source slice; expected is expected result after round-trip.
testWithSetup :: File -> String -> Maybe (Selector × File) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup (File file) expected v_expect_opt setup =
   before setup $
      it file \(γ × s) -> do
         let t × v = successful (desugarEval γ s)
             γ' × s' = desugarEval_bwd (t × s) (fromMaybe identity (fst <$> v_expect_opt) v)
             v' = desugarEval_fwd γ' s' t
         unless (isGraphical v') (checkPretty "Value" expected v')
         case snd <$> v_expect_opt of
            Nothing -> pure unit
            Just file_expect ->
               loadFile (Folder "fluid/example") file_expect >>= flip (checkPretty "Source selection") s'

test :: File -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: File -> File -> Selector -> String -> Test Unit
testBwd file file_expect δv expected =
   let folder = File "slicing/"
       file' = folder <> file in
   testWithSetup file' expected (Just (δv × (folder <> file_expect))) (openWithDefaultImports file')

testLink :: LinkFigSpec -> Val 𝔹 -> String -> Test Unit
testLink spec@{ x } v1' v2_expect =
   before (loadLinkFig spec) $
      it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
         \{ γ0, e2, t1, t2 } ->
            let { v': v2' } = successful $ linkResult x γ0 e2 t1 t2 v1' in
            checkPretty "Linked output" v2_expect v2'

testWithDataset :: File -> File -> Test Unit
testWithDataset dataset file = do
   testWithSetup file "" Nothing $ do
      γ0 × γ <- openDatasetAs dataset "data"
      (γ0 `concat` γ × _) <$> open file
