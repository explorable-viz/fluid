module Test.Puppeteer where

import Prelude

import Data.String as String
import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Toppokki as T
import Test.Assert as Assert

main :: Effect (Promise (Unit))
main = Promise.fromAff tests

isNotEmptyString :: String -> Boolean
isNotEmptyString str = not (str == "")

tests :: Aff Unit
tests = do
   browser <- T.launch {}
   page <- T.newPage browser
   T.goto (T.URL "http://127.0.0.1:8080") page
   content <- T.content page
   liftEffect (log content)
   liftEffect (Assert.assert' "Content is non-empty string" (String.length content > 0))
   --liftEffect (Assert.assertTrue' "Graph exists" (String.contains (String.Pattern "fig-4") content))
   T.close browser
   liftEffect (log "In Puppeteer.purs")

checkForFigures :: T.Page -> Aff Unit
checkForFigures page = do
   let selector = T.Selector "fig-4"
   let options = { visible: true }
   _ <- T.pageWaitForSelector selector options page
   pure unit
