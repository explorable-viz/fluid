module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports)
import Pretty (pretty, render)
import Util ((×), successful)
import Val (Val(..))
import Desugar (desugar, lcomp2, lcomp3, lcomp3_eval)
import Debug.Trace (trace) as T

trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a

runExample :: String -> String -> Boolean -> Effect Unit
runExample file expected runBwd = runMocha $
   before (openWithImports file) $
      it file $ \(ρ × e) -> do
         case successful $ eval ρ e of
            t × (Val _ u) -> do
               let fwd_v@(Val _ u') = eval_fwd ρ e true
               (render $ pretty u) `shouldEqual` (render $ pretty u')
               (render $ pretty u') `shouldEqual` expected
               if runBwd then
                  do let ρ' × e' × α' = eval_bwd fwd_v t
                         t' × v'      = successful $ eval ρ' e'
                     (render $ pretty t) `shouldEqual` (render $ pretty t')
                     (render $ pretty v') `shouldEqual` expected
               else pure unit

runDesugar :: String -> String -> Effect Unit
runDesugar file expected  = runMocha $
   before (openWithImports file) $
      it file $ \(ρ × _) -> do
         let k = trace (desugar lcomp3) 5
         case successful $ eval ρ (desugar lcomp3) of
            t × (Val _ u) -> do
               (render $ pretty u) `shouldEqual` expected


main :: Effect Unit
main = do
   runDesugar "arithmetic" lcomp3_eval
   -- runExample "arithmetic" "42" false
   -- runExample "compose" "5" false
   -- runExample "factorial" "40320" false
   -- runExample "filter" "[8, 7]" false
   -- runExample "foldr_sumSquares" "661" false
   -- runExample "lexicalScoping" "\"6\"" false
   -- runExample "length" "2" false
   -- runExample "map" "[5, 7, 13, 15, 4, 3, -3]" false
   -- runExample "normalise" "(33, 66)" false
   -- runExample "pattern-match" "4" false
   -- runExample "reverse" "[2, 1]" false
   -- runExample "zipWith" "[[10], [12], [20]]" false
   -- runExample "temp" "5" false
