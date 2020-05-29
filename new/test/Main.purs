module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Text.Parsing.Parser (runParser)
import Eval (eval)
import Fwd (eval_fwd)
import Module (loadFile, successfulParse)
import Parse (program)
import Pretty (pretty)
import Selected (Selected(..))
import Util (error)
import Val (primitives)

runExample :: String -> String -> Effect Unit
runExample file expected = runMocha $
   before (loadFile "fluid/example" file) $
      it file $ \text -> do
         let e = successfulParse text program
         let ρ = primitives
         let { u } = (eval ρ e).v
         let { u: u' } = eval_fwd ρ e Top
         (show $ pretty u) `shouldEqual` (show $ pretty u')
         (show $ pretty u') `shouldEqual` expected

main :: Effect Unit
main = do
   runExample "arithmetic" "42"
   runExample "compose" "5"
   runExample "normalise" "(33, 66)"
   runExample "temp" "(4, 3)"
