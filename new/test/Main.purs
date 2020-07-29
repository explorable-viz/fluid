module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import Desugar (SExpr, desugar)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports, loadModule)
import Pretty (pretty, render)
import Primitive (primitives)
import Util ((×), successful)
import Test.Desugar(lcomp1, lcomp2, lcomp3, lcomp4, lcomp1_eval, lcomp2_eval, lcomp3_eval, lcomp4_eval, lseq1, lseq1_eval)

runExample :: String -> String -> Boolean -> Effect Unit
runExample file expected slice = runMocha $
   before (openWithImports file) $
      it file $ \(ρ × e) -> do
         case successful $ eval ρ e of
            t × v -> do
               (render $ pretty v) `shouldEqual` expected
               if slice then do
                  let ρ' × e' × α'  = eval_bwd v t
                      v'            = eval_fwd ρ' e' true
                  (render $ pretty v') `shouldEqual` expected
               else pure unit

runDesugar :: String -> SExpr -> String -> Effect Unit
runDesugar test s expected = runMocha $
   before (loadModule "prelude" primitives) $
      it test $ \ρ ->
         case successful $ eval ρ (desugar s) of
            t × v -> (render $ pretty v) `shouldEqual` expected

main :: Effect Unit
main = do
   runDesugar "list-comp-1" lcomp1 lcomp1_eval
   runDesugar "list-comp-2" lcomp2 lcomp2_eval
   runDesugar "list-comp-3" lcomp3 lcomp3_eval
   runDesugar "list-comp-4" lcomp4 lcomp4_eval
   runDesugar "list-seq-1" lseq1 lseq1_eval
   runExample "arithmetic" "42" true
   runExample "compose" "5" true
   runExample "factorial" "40320" true
   runExample "filter" "[8, 7]" true
   runExample "flatten" "[(3, \"simon\"), (4, \"john\"), (6, \"sarah\"), (7, \"claire\")]" true
   runExample "foldr_sumSquares" "661" true
   runExample "lexicalScoping" "\"6\"" true
   runExample "length" "2" true
   runExample "map" "[5, 7, 13, 15, 4, 3, -3]" true
   runExample "normalise" "(33, 66)" true
   runExample "pattern-match" "4" true
   runExample "reverse" "[2, 1]" true
   runExample "zipWith" "[[10], [12], [20]]" true
   -- scratchpad
   runExample "temp" "5" true
