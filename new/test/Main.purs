module Test.Main where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Spec (before, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Eval (eval)
import Fwd (eval_fwd)
import Module (openWithImports)
import Pretty (pretty)
import Selected (Selected(..))
import Val (primitives)

runExample :: String -> String -> Effect Unit
runExample file expected = runMocha $
   before (openWithImports file) $
      it file $ \(Tuple ρ e) -> do
         let ρ' = ρ <> primitives
         let { u } = (eval ρ' e).v
         let { u: u' } = eval_fwd ρ' e Top
         (show $ pretty u) `shouldEqual` (show $ pretty u')
         (show $ pretty u') `shouldEqual` expected

main :: Effect Unit
main = do
   runExample "arithmetic" "42"
   runExample "compose" "5"
   runExample "factorial" "40320"
   runExample "normalise" "(33, 66)"

   runExample "temp" "(4, 3)"
