module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Text.Parsing.Parser (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Eval (eval)
import Module (loadFile)
import Parse (program)
import Pretty (pretty)
import Util (error)
import Val (primitives)

{-
main :: Effect Unit
main = do
   test_normalise
-}

main :: Effect Unit
main = runMocha do
  describe "your feature" do
    it "works" $
      (1 + 1) `shouldEqual` 2

test_normalise :: Effect Unit
test_normalise = do
   log "Starting."
   launchAff_ do
      text <- loadFile "fluid/example" "normalise"
      liftEffect do
         log text
         runMocha do
            liftEffect $ log "Parsing"
            let result = runParser text program
            describe "Parse" do
               it "blah" do
                  case result of
                     Left parseError -> do
                        liftEffect $ log "Parse error."
                        error $ show parseError
                     Right e -> do
                        liftEffect $ log "Parsed ok."
                        let { u } = (eval primitives e).v
                        (show $ pretty u) `shouldEqual` "(492, 984)"
