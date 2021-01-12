module Test.Main where

import Prelude
import Data.Bitraversable (bitraverse)
import Data.Tuple (uncurry)
import Debug.Trace (trace) as T
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import DataType (dataTypeFor, typeName)
import DesugarBwd (desugarBwd)
import DesugarFwd (desugarFwd)
import Eval (eval)
import Fwd (eval_fwd)
import Lattice (𝔹)
import Module (openDatasetAs, openWithImports)
import Pretty (pretty, render)
import SExpr (Expr) as S
import Util (type (×), (×), successful)
import Val (Env, Val(..), RawVal(..))

trace :: forall a b . a -> b -> b
trace a b = T.trace a (\_ -> b)

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical Hole                 = false
isGraphical (Val _ (Constr c _)) = typeName (successful $ dataTypeFor c) == "GraphicsElement"
isGraphical (Val _ _)            = false

-- whether slicing is currently enabled in the tests
slicing :: Boolean
slicing = true

run :: forall a . SpecT Aff Unit Effect a → Effect Unit
run = runMocha -- nicer name

test' :: String -> Aff (Env 𝔹 × S.Expr 𝔹) -> String -> SpecT Aff Unit Effect Unit
test' name setup expected =
   before setup $
      it name $ \(ρ × s) -> do
         let e = successful $ desugarFwd s
         case successful $ eval ρ e of
            t × v -> do
               unless (isGraphical v) $
                  trace ((render $ pretty s) <> "\n" <> (render $ pretty e)) $
                  (render $ pretty v) `shouldEqual` expected
               when slicing do
                  let ρ' × e' × α'  = eval_bwd v t
                      v'            = eval_fwd ρ' e' true
                      s'            = successful $ desugarBwd e' s
                  unless (isGraphical v) $
                     (render $ pretty v') `shouldEqual` expected

test :: String -> String -> SpecT Aff Unit Effect Unit
test file = test' file (openWithImports file)

testWithDataset :: String -> String -> SpecT Aff Unit Effect Unit
testWithDataset dataset file =
   flip (test' file) "" $
      bitraverse (uncurry openDatasetAs) openWithImports (dataset × "data" × file) <#>
      (\(ρ × (ρ' × e)) -> (ρ <> ρ') × e)

main :: Effect Unit
main = do
   -- desugaring
   run $ test "desugar/list-comp-1" "[14, 12, 10, 13, 11, 9, 12, 10, 8]"
   run $ test "desugar/list-comp-2" "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]"
   run $ test "desugar/list-comp-3" "[9, 8]"
   run $ test "desugar/list-comp-4" "[5, 4, 3]"
   run $ test "desugar/list-comp-5" "[5, 4, 3]"
   run $ test "desugar/list-enum" "[3, 4, 5, 6, 7]"
   -- slicing
   run $ test "arithmetic" "42"
   run $ test "compose" "5"
   run $ test "factorial" "40320"
   run $ test "filter" "[8, 7]"
   run $ test "flatten" "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]"
   run $ test "foldr_sumSquares" "661"
   run $ test "lexicalScoping" "\"6\""
   run $ test "length" "2"
   run $ test "lookup" "Some \"sarah\""
   run $ test "map" "[5, 7, 13, 15, 4, 3, -3]"
   run $ test "mergeSort" "[1, 2, 3]"
   run $ test "normalise" "(33, 66)"
   run $ test "pattern-match" "4"
   run $ test "reverse" "[2, 1]"
   run $ test "zipWith" "[[10], [12], [20]]"
   -- graphics
   run $ testWithDataset "renewables-restricted" "graphics/background"
   run $ testWithDataset "renewables-restricted" "graphics/grouped-bar-chart"
   run $ testWithDataset "renewables-restricted" "graphics/line-chart"
   run $ testWithDataset "renewables-restricted" "graphics/stacked-bar-chart"
   -- scratchpad
   run $ test "temp" "[3, 4, 5, 6, 7]"
