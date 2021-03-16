module Test.Main where

import Prelude
import Data.Bitraversable (bitraverse)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
-- import Debug.Trace (trace) as T
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import DataType (dataTypeFor, typeName)
import DesugarBwd (desugarBwd)
import DesugarFwd (desugarFwd)
import Expr (Expr(..)) as E
import Eval (eval)
import EvalBwd (eval_bwd)
import EvalFwd (eval_fwd)
import Expl (Expl)
import Lattice (𝔹, botOf)
import Module (openDatasetAs, openWithDefaultImports)
import Pretty (pretty, render)
import SExpr (Expr) as S
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical Hole           = false
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) == "GraphicsElement"
isGraphical _              = false

-- whether slicing is currently enabled in the tests
slicing :: Boolean
slicing = true

run :: forall a . SpecT Aff Unit Effect a → Effect Unit
run = runMocha -- nicer name

desugarEval :: Env 𝔹 -> S.Expr 𝔹 -> MayFail (Expl 𝔹 × Val 𝔹)
desugarEval ρ s = desugarFwd s >>= eval ρ

desugarEval_bwd :: Expl 𝔹 × S.Expr 𝔹 -> Val 𝔹 -> Env 𝔹 × S.Expr 𝔹
desugarEval_bwd (t × s) v = let ρ × e × _ = eval_bwd v t in ρ × desugarBwd e s

desugarEval_fwd :: Env 𝔹 -> S.Expr 𝔹 -> Expl 𝔹 -> Val 𝔹
desugarEval_fwd ρ s =
   let _ = eval_fwd (botOf ρ) E.Hole true in -- sanity-check that this is defined
   eval_fwd ρ (successful (desugarFwd s)) true

test' :: String -> Aff (Env 𝔹 × S.Expr 𝔹) -> String -> SpecT Aff Unit Effect Unit
test' name setup expected =
   before setup $
      it name $ \(ρ × s) -> do
         case successful (desugarEval ρ s) of
            t × v -> do
               unless (isGraphical v) $
                  render (pretty v) `shouldEqual` expected
               when slicing do
                  let ρ' × s' = desugarEval_bwd (t × s) v
                      v' = desugarEval_fwd ρ' s' t
                  unless (isGraphical v) $
                     render (pretty v') `shouldEqual` expected

test :: String -> String -> SpecT Aff Unit Effect Unit
test file = test' file (openWithDefaultImports file)

testWithDataset :: String -> String -> SpecT Aff Unit Effect Unit
testWithDataset dataset file =
   flip (test' file) "" $
      bitraverse (uncurry openDatasetAs) openWithDefaultImports (dataset × "data" × file) <#>
      (\(ρ × (ρ' × e)) -> (ρ <> ρ') × e)

main :: Effect Unit
main = void $ sequence $ run <$> [
   test "desugar/list-comp-1" "[14, 12, 10, 13, 11, 9, 12, 10, 8]",
   test "desugar/list-comp-2" "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]",
   test "desugar/list-comp-3" "[9, 8]",
   test "desugar/list-comp-4" "[5, 4, 3]",
   test "desugar/list-comp-5" "[5, 4, 3]",
   test "desugar/list-comp-6" "[5]",
   test "desugar/list-comp-7" "[[]]",
   test "desugar/list-enum" "[3, 4, 5, 6, 7]",
   -- misc
   test "arithmetic" "42",
   test "array" "(1, (3, 3))",
   test "compose" "5",
   test "factorial" "40320",
   test "filter" "[8, 7]",
   test "flatten" "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]",
   test "foldr_sumSquares" "661",
   test "lexicalScoping" "\"6\"",
   test "length" "2",
   test "lookup" "Some \"sarah\"",
   test "map" "[5, 7, 13, 15, 4, 3, -3]",
   test "mergeSort" "[1, 2, 3]",
   test "normalise" "(33, 66)",
   test "pattern-match" "4",
   test "reverse" "[2, 1]",
   test "zipWith" "[[10], [12], [20]]",
   -- graphics
   testWithDataset "renewables-restricted" "graphics/background",
   testWithDataset "renewables-restricted" "graphics/grouped-bar-chart",
   testWithDataset "renewables-restricted" "graphics/line-chart",
   testWithDataset "renewables-restricted" "graphics/stacked-bar-chart",
   -- scratchpad
   test "temp" "[[10], [12], [20]]"
]
