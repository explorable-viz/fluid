module Test.Util where

import Prelude hiding (absurd)
import Data.List (List(..), (:), elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bindings ((↦))
import DataType (cBarChart, cCons, cPair, dataTypeFor, typeName)
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
import Util (MayFail, type (×), (×), successful)
import Util.SnocList (SnocList(..), (:-), splitAt)
import Val (Env, Val(..), holeMatrix, insertMatrix)

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

checkPretty :: forall a . Pretty a => a -> String -> Aff Unit
checkPretty x expected = prettyP x `shouldEqual` expected

-- v_opt is output slice; expected is expected result after round-trip.
testWithSetup :: File -> String -> Maybe (Val 𝔹) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup (File file) expected v_opt setup =
   before setup $
      it file \(ρ × s) -> do
         let t × v = successful (desugarEval ρ s)
             ρ' × s' = desugarEval_bwd (t × s) (fromMaybe v v_opt)
             v = desugarEval_fwd ρ' s' t
         unless (isGraphical v) (checkPretty v expected)
         case v_opt of
            Nothing -> pure unit
            Just _ -> loadFile (Folder "fluid/example") (File $ file <> ".expect") >>= checkPretty s'

test :: File -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: File -> Val 𝔹 -> String -> Test Unit
testBwd (File file) v expected =
   let file' = File ("slicing/" <> file) in
   testWithSetup file' expected (Just v) (openWithDefaultImports file')

type LinkConfig = {
   ρ0 :: Env 𝔹,      -- ambient env (default imports)
   ρ :: Env 𝔹,       -- additional singleton env for dataset
   s1 :: S.Expr 𝔹,   -- view 1
   s2 :: S.Expr 𝔹    -- view 2
}

testLink :: File -> File -> File -> Val 𝔹 -> String -> Test Unit
testLink (File file1) (File file2) (File dataFile) v1_sel v2_expect =
   let dir = "linking/"
       name1 × name2 = File (dir <> file1) × File (dir <> file2)
       setup = do
         -- the views share an ambient environment ρ0 as well as dataset
         ρ0 × ρ <- openDatasetAs (File $ "example/" <> dir <> dataFile) "data"
         s1 <- open name1
         s2 <- open name2
         pure { ρ0, ρ, s1, s2 } :: Aff LinkConfig in
   before setup $
      it (dir <> file1 <> " <-> " <> file2) \{ ρ0, ρ, s1, s2 } -> do
         let e1 = successful (desugarFwd s1)
             e2 = successful (desugarFwd s2)
             t1 × v1 = successful (eval (ρ0 <> ρ) e1)
             t2 × v2 = successful (eval (ρ0 <> ρ) e2)
             ρ0ρ × _ × _ = evalBwd v1_sel t1
             _ × ρ' = splitAt 1 ρ0ρ
             -- make ρ0 and e2 fully available; ρ0 is too big to operate on, so we use (topOf ρ0)
             -- combined with the negation of the dataset environment slice
             v2' = neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> e2) true t2)
         checkPretty v2' v2_expect

testWithDataset :: File -> File -> Test Unit
testWithDataset dataset file = do
   testWithSetup file "" Nothing $ do
      ρ0 × ρ <- openDatasetAs dataset "data"
      let ρ' = ρ0 <> ρ
      (ρ' × _) <$> open file

-- Selection helpers.
selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix false (insertMatrix i j (Hole true) (holeMatrix i' j'))

selectNth :: Int -> Val 𝔹 -> Val 𝔹
selectNth 0 v = Constr false cCons (v : Hole false : Nil)
selectNth n v = Constr false cCons (Hole false : selectNth (n - 1) v : Nil)

select_y :: Val 𝔹
select_y = Record false (Lin :- "x" ↦ Hole false :- "y" ↦ Hole true)

selectBarChart_data :: Val 𝔹 -> Val 𝔹
selectBarChart_data v = Constr false cBarChart (Record false (Lin :- "caption" ↦ Hole false :- "data" ↦ v) : Nil)

selectPair :: 𝔹 -> Val 𝔹 -> Val 𝔹 -> Val 𝔹
selectPair α v1 v2 = Constr α cPair (v1 : v2 : Nil)
