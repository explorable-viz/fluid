module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Module (loadFile)


main :: Effect Unit
main = runMocha $
   describe "feature" $
      it "works" $
         (2 + 3) `shouldEqual` 5

test :: Spec Unit
test =
   before (loadFile "fluid/example" "normalise") do
      describe "feature" $
         it "works" $ \text ->
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
