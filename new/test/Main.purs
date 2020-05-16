module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Text.Parsing.Parser (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Eval (eval)
import Module (loadFile)
import Parse (program)
import Pretty (pretty)
import Util (error)
import Val (primitives)

main :: Effect Unit
main = do
   test_normalise

test_normalise :: Effect Unit
test_normalise = do
   launchAff_ do
      text <- loadFile "fluid/example" "normalise"
      let result = runParser text program
      runSpec [consoleReporter] do
         describe "Parse" do
            it "blah" do
               case result of
                  Left parseError -> do
                     error $ show parseError
                  Right e -> do
                     let { u } = (eval primitives e).v
                     (show $ pretty u) `shouldEqual` "(492, 984)"
