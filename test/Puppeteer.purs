module Test.Puppeteer where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
--import Test.Assert (assertEqual)
import Effect.Class (liftEffect)
import Effect.Console (log)
--import Test.Assert as Assert
import Toppokki as T

--import Data.String as String

-- Function to check if a string is non-empty
isNotEmpty :: String -> Boolean
isNotEmpty str = String.length str > 0

main :: Effect Unit
main = do
   launchAff_ do
      browser <- T.launch {}
      page <- T.newPage browser
      T.goto (T.URL "http://127.0.0.1:8080") page
      content <- T.content page
      --liftEffect (Assert.assert' "Content is non-empty string" (String.length content > 0))
      liftEffect (log content)
      --liftEffect (Assert.assertTrue' "Graph exists" (String.contains (String.Pattern "fig-4") content))
      T.close browser
   log "hello"

------------
{-
foreign import isGraphPresent :: String -> String -> Aff Boolean

checkGraphPresence :: String -> String -> Aff Boolean
checkGraphPresence url selector = isGraphPresent url selector

testGraphPresence :: Effect Unit
testGraphPresence = launchAff_ do
  let url = "http://127.0.0.1:8080"
  let selector = "#graph"

  isPresent <- checkGraphPresence url selector

  --Assert.equal isPresent true "Graph should be present"
  assertEqual "Graph should be present" true isPresent

  log $ if isPresent
    then "Graph is present."
    else "Graph is not present."
-}