module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports)
import Pretty (pretty, render)
import Util (error)
import Val (Val(..))

runExample :: String -> String -> Effect Unit
runExample file expected = runMocha $
   before (openWithImports file) $
      it file $ \(Tuple ρ e) -> do
         case eval ρ e of
            Left msg -> error msg
            Right (Tuple _ (Val _ u)) -> do
               let (Val _ u') = eval_fwd ρ e true
               (render $ pretty u) `shouldEqual` (render $ pretty u')
               (render $ pretty u') `shouldEqual` expected

main :: Effect Unit
main = do
   runExample "arithmetic" "42"
   runExample "compose" "5"
   runExample "factorial" "40320"
   runExample "foldr_sumSquares" "661"
   runExample "lexicalScoping" "\"6\""
   runExample "length" "2"
   runExample "map" "[5, 7, 13, 15, 4, 3, -3]"
   runExample "normalise" "(33, 66)"

--   runExample "temp" "7"
