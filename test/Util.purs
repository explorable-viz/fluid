module Test.Util where

import Prelude hiding (absurd)
import Data.Bitraversable (bitraverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import DataType (dataTypeFor, typeName)
import DesugarBwd (desugarBwd)
import DesugarFwd (desugarFwd)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Expl (Expl)
import Expr (Expr(..)) as E
import SExpr (Expr) as S
import Lattice (𝔹, botOf)
import Module (openDatasetAs, openWithDefaultImports)
import Pretty (class Pretty, pretty, render)
import Util (MayFail, type (×), (×), successful, unzip)
import Val (Env, Val(..))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical Hole           = false
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) == "GraphicsElement"
isGraphical _              = false

type Test a = SpecT Aff Unit Effect a

run :: forall a . Test a → Effect Unit
run = runMocha -- no reason at all to have to look at the word "Mocha"

desugarEval :: Env 𝔹 -> S.Expr 𝔹 -> MayFail (Expl 𝔹 × Val 𝔹)
desugarEval ρ s = desugarFwd s >>= eval ρ

desugarEval_bwd :: Expl 𝔹 × S.Expr 𝔹 -> Val 𝔹 -> Env 𝔹 × S.Expr 𝔹
desugarEval_bwd (t × s) v = let ρ × e × _ = evalBwd v t in ρ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Expl 𝔹 -> Val 𝔹
desugarEval_fwd ρ s =
   let _ = evalFwd (botOf ρ) E.Hole true in -- sanity-check that this is defined
   evalFwd ρ (successful (desugarFwd s)) true

checkPretty :: forall a . Pretty a => a -> String -> Aff Unit
checkPretty x expected = render (pretty x) `shouldEqual` expected

-- bwd_opt is pair of (output slice, string representation of expected program slice)
testWithSetup :: String -> String -> Maybe (Val 𝔹 × String) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup name v_str bwd_opt setup =
   let v_opt × s_str_opt = unzip bwd_opt in
   before setup $
      it name $ \(ρ × s) -> do
         let t × v = successful (desugarEval ρ s)
             ρ' × s' = desugarEval_bwd (t × s) (fromMaybe v v_opt)
             v = desugarEval_fwd ρ' s' t
         unless (isGraphical v) (checkPretty v v_str)
         case s_str_opt of
            Nothing -> pure unit
            Just s_str -> checkPretty s' s_str

test :: String -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

test_bwd :: String -> (Val 𝔹 × String) -> String -> Test Unit
test_bwd file v_str expected = testWithSetup file expected (Just v_str) (openWithDefaultImports file)

testWithDataset :: String -> String -> Test Unit
testWithDataset dataset file =
   testWithSetup file "" Nothing $
      bitraverse (uncurry openDatasetAs) openWithDefaultImports (dataset × "data" × file) <#>
      (\(ρ × (ρ' × e)) -> (ρ <> ρ') × e)
