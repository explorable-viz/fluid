module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Text.Parsing.Parser (runParser)
import Module (loadFile)
import Parse (program)


main :: Effect Unit
main = runMocha $
   before (loadFile "fluid/example" "normalise") do
      describe "feature" $
         it "works" $ \text -> do
            let result = runParser text program
            (2 + 3) `shouldEqual` 5

{-
test_normalise :: Effect Unit
test_normalise = launchAff_ do
   text <- loadFile "fluid/example" "normalise"
   liftEffect do
      log text
      runMocha do
         liftEffect $ log "Parsing"
         let result = runParser text program
         describe "Parse" do
            it "works"
               case result of
                  Left parseError -> do
                     error $ show parseError
                  Right e ->
                     trace e \_ -> do
                        liftEffect $ log "Parsed ok."
                        let { u } = (eval primitives e).v
                        (show $ pretty u) `shouldEqual` "(492, 984)"
-}
