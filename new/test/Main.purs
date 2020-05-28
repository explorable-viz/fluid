module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Text.Parsing.Parser (runParser)
import Eval (eval)
import Fwd (eval_fwd)
import Module (loadFile)
import Parse (program)
import Pretty (pretty)
import Selected (Selected(..))
import Util (error)
import Val (primitives)

runExample :: String -> String -> Effect Unit
runExample file expected = runMocha $
   before (loadFile "fluid/example" file) $
      describe "feature" $
         it "works" $ \text -> do
            let result = runParser text program
            case result of
               Left parseError -> do
                  error $ show parseError
               Right e -> do
                  let ρ = primitives
                  let { u } = (eval ρ e).v
                  let { u: u' } = eval_fwd ρ e Top
                  (show $ pretty u) `shouldEqual` (show $ pretty u')
                  (show $ pretty u') `shouldEqual` expected

main :: Effect Unit
main = do
   runExample "normalise" "(33, 66)"
   runExample "temp" "(4, 3)"
