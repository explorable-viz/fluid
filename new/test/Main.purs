module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import Desugar (SExpr, desugar, lcomp1, lcomp2, lcomp3, lcomp1_eval, lcomp2_eval, lcomp3_eval, lseq1, lseq1_eval)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports, loadModule)
import Pretty (pretty, render)
import Primitive (primitives)
import Util ((×), successful)

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
                  (render $ pretty e') `shouldEqual` "blah"
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
   runDesugar "list-seq-1" lseq1 lseq1_eval
   runExample "arithmetic" "42" true
   runExample "compose" "5" false
   runExample "factorial" "40320" false
   runExample "filter" "[8, 7]" false
   runExample "foldr_sumSquares" "661" false
   runExample "lexicalScoping" "\"6\"" false
   runExample "length" "2" false
   runExample "map" "[5, 7, 13, 15, 4, 3, -3]" false
   runExample "normalise" "(33, 66)" false
   runExample "pattern-match" "4" false
   runExample "reverse" "[2, 1]" false
   runExample "zipWith" "[[10], [12], [20]]" false
   runExample "temp" "5" false
