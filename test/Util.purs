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
import Module (loadFile, openDatasetAs, openWithDefaultImports)
import Pretty (class Pretty, prettyP)
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical (Hole _)       = false
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) == "GraphicsElement"
isGraphical _              = false

type Test a = SpecT Aff Unit Effect a

run :: forall a . Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

desugarEval :: Env 𝔹 -> S.Expr 𝔹 -> MayFail (Expl 𝔹 × Val 𝔹)
desugarEval ρ s = desugarFwd s >>= eval ρ

desugarEval_bwd :: Expl 𝔹 × S.Expr 𝔹 -> Val 𝔹 -> Env 𝔹 × S.Expr 𝔹
desugarEval_bwd (t × s) v = let ρ × e × _ = evalBwd v t in ρ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Expl 𝔹 -> Val 𝔹
desugarEval_fwd ρ s =
   let _ = evalFwd (botOf ρ) (E.Hole false) false in -- sanity-check that this is defined
   evalFwd ρ (successful (desugarFwd s)) true

checkPretty :: forall a . Pretty a => a -> String -> Aff Unit
checkPretty x expected = prettyP x `shouldEqual` expected

-- v_opt is output slice
testWithSetup :: String -> String -> Maybe (Val 𝔹) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup name v_str v_opt setup =
   before setup $
      it name \(ρ × s) -> do
         let t × v = successful (desugarEval ρ s)
             ρ' × s' = desugarEval_bwd (t × s) (fromMaybe v v_opt)
             v = desugarEval_fwd ρ' s' t
         unless (isGraphical v) (checkPretty v v_str)
         case v_opt of
            Nothing -> pure unit
            Just _ -> loadFile "fluid/example" (name <> ".expect") >>= checkPretty s'

test :: String -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: String -> Val 𝔹 -> String -> Test Unit
testBwd file v expected = testWithSetup file expected (Just v) (openWithDefaultImports file)

testLink :: String -> Test Unit
testLink file =
   let name = "linking/" <> file
       blah1 = openWithDefaultImports (name <> "-data") :: Aff (Env 𝔹 × S.Expr 𝔹)
       blah2 = openWithDefaultImports (name <> "-1") :: Aff (Env 𝔹 × S.Expr 𝔹)
       blah3 = openWithDefaultImports (name <> "-2") :: Aff (Env 𝔹 × S.Expr 𝔹) in
   it name \_ -> do
      pure unit

testWithDataset :: String -> String -> Test Unit
testWithDataset dataset file =
   testWithSetup file "" Nothing $
      bitraverse (uncurry openDatasetAs) openWithDefaultImports (dataset × "data" × file) <#>
      (\(ρ × (ρ' × e)) -> (ρ <> ρ') × e)
