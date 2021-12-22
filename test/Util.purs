module Test.Util where

import Prelude hiding (absurd)
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bindings (Var, find)
import DataType (dataTypeFor, typeName)
import DesugarBwd (desugarBwd)
import DesugarFwd (desugarFwd)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Expl (Expl)
import Expr (Expr(..)) as E
import SExpr (Expr) as S
import Lattice (𝔹, botOf, neg)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openWithDefaultImports)
import Pretty (class Pretty, prettyP)
import Primitive (Slice)
import Util (MayFail, type (×), (×), successful)
import Util.SnocList (splitAt)
import Val (Env, Val(..))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical (Hole _)       = false
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` ["GraphicsElement", "Plot"]
isGraphical _              = false

type Test a = SpecT Aff Unit Effect a

run :: forall a . Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

desugarEval :: Env 𝔹 -> S.Expr 𝔹 -> MayFail (Expl 𝔹 × Val 𝔹)
desugarEval ρ s = desugarFwd s >>= eval ρ

desugarEval_bwd :: Expl 𝔹 × S.Expr 𝔹 -> Val 𝔹 -> Env 𝔹 × S.Expr 𝔹
desugarEval_bwd (t × s) v =
   let ρ × e × _ = evalBwd v t in
   ρ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Expl 𝔹 -> Val 𝔹
desugarEval_fwd ρ s =
   let _ = evalFwd (botOf ρ) (E.Hole false) false in -- sanity-check that this is defined
   evalFwd ρ (successful (desugarFwd s)) true

checkPretty :: forall a . Pretty a => String -> String -> a -> Aff Unit
checkPretty msg expected x =
   trace (msg <> ":\n" <> prettyP x) \_ ->
      prettyP x `shouldEqual` expected

-- v_expect_opt is optional output slice + expected source slice; expected is expected result after round-trip.
testWithSetup :: File -> String -> Maybe (Val 𝔹 × File) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup (File file) expected v_expect_opt setup =
   before setup $
      it file \(ρ × s) -> do
         let t × v = successful (desugarEval ρ s)
             ρ' × s' = desugarEval_bwd (t × s) (fromMaybe v (fst <$> v_expect_opt))
             v = desugarEval_fwd ρ' s' t
         unless (isGraphical v) (checkPretty "Value" expected v)
         case snd <$> v_expect_opt of
            Nothing -> pure unit
            Just file_expect -> loadFile (Folder "fluid/example") file_expect >>= flip (checkPretty "Source selection") s'

test :: File -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: File -> File -> Val 𝔹 -> String -> Test Unit
testBwd file file_expect v expected =
   let folder = File "slicing/"
       file' = folder <> file in
   testWithSetup file' expected (Just (v × (folder <> file_expect))) (openWithDefaultImports file')

type LinkConfig = {
   file1 :: File,
   file2 :: File,
   dataFile :: File,
   dataVar :: Var,
   v1_sel :: Val 𝔹
}

type LinkResult = {
   v1 :: Val 𝔹,             -- original value of view 1
   v2 :: Slice (Val 𝔹),
   data_sel :: Slice (Val 𝔹)
}

doLink :: LinkConfig -> Aff LinkResult
doLink { file1, file2, dataFile, dataVar: x, v1_sel } = do
   let dir = File "linking/"
       name1 × name2 = (dir <> file1) × (dir <> file2)
   -- the views share an ambient environment ρ0 as well as dataset
   ρ0 × ρ <- openDatasetAs (File "example/" <> dir <> dataFile) x
   s1 <- open name1
   s2 <- open name2
   pure $ successful do
      e1 <- desugarFwd s1
      e2 <- desugarFwd s2
      t1 × v1 <- eval (ρ0 <> ρ) e1
      t2 × v2 <- eval (ρ0 <> ρ) e2
      let ρ0ρ × _ × _ = evalBwd v1_sel t1
          _ × ρ' = splitAt 1 ρ0ρ
      v <- find x ρ
      v' <- find x ρ'
      -- make ρ0 and e2 fully available; ρ0 is too big to operate on, so we use (topOf ρ0)
      -- combined with the negation of the dataset environment slice
      pure {
         v1: v1,
         v2: neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> e2) true t2) × v2,
         data_sel: v' × v
      }

testLink :: LinkConfig -> String -> Test Unit
testLink config v2_expect =
   before (doLink config) $
      it ("linking/" <> show config.file1 <> " <-> " <> show config.file2)
         \{ v2: v2' × _ } ->
            checkPretty "Linked output" v2_expect v2'

testWithDataset :: File -> File -> Test Unit
testWithDataset dataset file = do
   testWithSetup file "" Nothing $ do
      ρ0 × ρ <- openDatasetAs dataset "data"
      let ρ' = ρ0 <> ρ
      (ρ' × _) <$> open file
