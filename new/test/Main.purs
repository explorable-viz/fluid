module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import Bindings (Var)
import DataType (dataTypeFor, typeName)
import Desugar (SExpr, desugar)
import Eval (eval)
import Fwd (eval_fwd)
import Lattice (𝔹)
import Module (loadModule, openDatasetAs, openWithImports)
import Pretty (pretty, render)
import Primitive (primitives)
import Util ((×), successful)
import Val (Env, Val(..), RawVal(..))
import Test.Desugar(lcomp1, lcomp2, lcomp3, lcomp4, lcomp1_eval, lcomp2_eval, lcomp3_eval, lcomp4_eval, lseq1, lseq1_eval)

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a . Val a -> Boolean
isGraphical Hole                 = false
isGraphical (Val _ (Constr c _)) = typeName (successful $ dataTypeFor c) == "GraphicsElement"
isGraphical (Val _ _)            = false

run :: forall a . SpecT Aff Unit Effect a → Effect Unit
run = runMocha -- nicer name

withDataset :: String -> Var -> forall m a . Monad m => SpecT Aff (Env 𝔹) m a → SpecT Aff Unit m a
withDataset file x = before (openDatasetAs file x)

test :: String -> String -> Boolean -> SpecT Aff Unit Effect Unit
test file expected slice =
   before (openWithImports file) $
      it file $ \(ρ × e) -> do
         case successful $ eval ρ e of
            t × v -> do
               unless (isGraphical v) $
                  (render $ pretty v) `shouldEqual` expected
               when slice do
                  let ρ' × e' × α'  = eval_bwd v t
                      v'            = eval_fwd ρ' e' true
                  unless (isGraphical v) $
                     (render $ pretty v') `shouldEqual` expected

desugarTest :: String -> SExpr -> String -> SpecT Aff Unit Effect Unit
desugarTest name s expected =
   before (loadModule "prelude" primitives) $
      it name $ \ρ ->
         case successful $ eval ρ (desugar s) of
            t × v -> (render $ pretty v) `shouldEqual` expected

main :: Effect Unit
main = do
   -- desugaring
   run $ desugarTest "list-comp-1" lcomp1 lcomp1_eval
   run $ desugarTest "list-comp-2" lcomp2 lcomp2_eval
   run $ desugarTest "list-comp-3" lcomp3 lcomp3_eval
   run $ desugarTest "list-comp-4" lcomp4 lcomp4_eval
   run $ desugarTest "list-seq-1" lseq1 lseq1_eval
   -- slicing
   run $ test "arithmetic" "42" true
   run $ test "compose" "5" true
   run $ test "factorial" "40320" true
   run $ test "filter" "[8, 7]" true
   run $ test "flatten" "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]" true
   run $ test "foldr_sumSquares" "661" true
   run $ test "lexicalScoping" "\"6\"" true
   run $ test "length" "2" true
   run $ test "lookup" "Some \"sarah\"" true
   run $ test "map" "[5, 7, 13, 15, 4, 3, -3]" true
   run $ test "mergeSort" "[1, 2, 3]" true
   run $ test "normalise" "(33, 66)" true
   run $ test "pattern-match" "4" true
   run $ test "reverse" "[2, 1]" true
   run $ test "zipWith" "[[10], [12], [20]]" true
   -- graphics
   run $ test "graphics/background" "" true
   run $ test "graphics/line-chart" "" true
   -- scratchpad
   run $ test "temp" "5.2" true
