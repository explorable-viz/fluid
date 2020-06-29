module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Bwd (eval_bwd)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports)
import Pretty (pretty, render)
import Util ((×), error)
import Val (Val(..))

runExample :: String -> String -> Effect Unit
runExample file expected = runMocha $
   before (openWithImports file) $
      it file $ \(ρ × e) -> do
         case eval ρ e of
            Left msg -> error msg
            Right (_ × (Val _ u)) -> do
               let Val _ u' = eval_fwd ρ e true
               (render $ pretty u) `shouldEqual` (render $ pretty u')
               (render $ pretty u') `shouldEqual` expected

runExampleBwd :: String -> String -> Effect Unit
runExampleBwd file expected = runMocha $
   before (openWithImports file) $
      it file $ \(ρ × e) -> do
         case eval ρ e of
            Left msg -> error msg
            Right (t × v) -> do
               let fwd_v = eval_fwd ρ e true
                   ρ' × e' × α = eval_bwd fwd_v t
               case eval ρ' e' of
                    Left msg -> error msg
                    Right (t' × v') -> do
                        (render $ pretty t) `shouldEqual` (render $ pretty t')
                        (render $ pretty v') `shouldEqual` expected

main :: Effect Unit
main = do
   runExampleBwd "arithmetic" "42"
   runExampleBwd "compose" "5"
   runExampleBwd "factorial" "40320"
   runExampleBwd "filter" "[8, 7]"
   runExampleBwd "foldr_sumSquares" "661"
   runExampleBwd "lexicalScoping" "\"6\""
   runExampleBwd "length" "2"
   runExampleBwd "map" "[5, 7, 13, 15, 4, 3, -3]"
   runExampleBwd "normalise" "(33, 66)"
   runExampleBwd "pattern-match" "4"
   runExampleBwd "reverse" "[2, 1]"
   runExampleBwd "zipWith" "[(3, 10), (4, 12), (8, 20)]"
   runExampleBwd "temp" "5"
