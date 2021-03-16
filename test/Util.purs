module Test.Util where

import Prelude
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
import EvalBwd (eval_bwd)
import EvalFwd (eval_fwd)
import Expl (Expl)
import Expr (Expr(..)) as E
import SExpr (Expr) as S
import Lattice (𝔹, botOf)
import Module (openDatasetAs, openWithDefaultImports)
import Pretty (pretty, render)
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
desugarEval_bwd (t × s) v = let ρ × e × _ = eval_bwd v t in ρ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Expl 𝔹 -> Val 𝔹
desugarEval_fwd ρ s =
   let _ = eval_fwd (botOf ρ) E.Hole true in -- sanity-check that this is defined
   eval_fwd ρ (successful (desugarFwd s)) true

testWithSetup :: String -> String -> Maybe (Val 𝔹 × String) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup name expected v_str_opt setup =
   let v_opt × str_opt = unzip v_str_opt in
   before setup $
      it name $ \(ρ × s) ->
         let t × v = successful (desugarEval ρ s)
             ρ' × s' = desugarEval_bwd (t × s) (fromMaybe v v_opt) in
         checkExpected (desugarEval_fwd ρ' s' t)
   where
   checkExpected :: Val 𝔹 -> Aff Unit
   checkExpected v = unless (isGraphical v) (render (pretty v) `shouldEqual` expected)

test :: String -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

test_bwd :: String -> (Val 𝔹 × String) -> String -> Test Unit
test_bwd file v_str expected = testWithSetup file expected (Just v_str) (openWithDefaultImports file)

testWithDataset :: String -> String -> Test Unit
testWithDataset dataset file =
   testWithSetup file "" Nothing $
      bitraverse (uncurry openDatasetAs) openWithDefaultImports (dataset × "data" × file) <#>
      (\(ρ × (ρ' × e)) -> (ρ <> ρ') × e)
