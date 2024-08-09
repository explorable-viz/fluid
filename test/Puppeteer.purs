module Test.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Toppokki as T
import Util (check)

main :: Effect (Promise (Unit))
main = Promise.fromAff tests

tests :: Aff Unit
tests = do
   browser <- T.launch {}
   page <- T.newPage browser
   T.goto (T.URL "http://127.0.0.1:8080") {} page
   --_ <- T.waitForNavigation { waitUntil: T.networkIdle2 } page
   content <- T.content page
   liftEffect (log content)
   check (String.length content > 0) "Content is non-empty string"
   checkForFigure page "fig-4"
   checkForFigure page "fig-1"
   checkForFigure page "fig-conv-2"
   T.close browser

checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = T.Selector ("div#" <> id)
   let options = { visible: true, timeout: 300000 }
   _ <- T.pageWaitForSelector selector options page
   liftEffect (log ("Found " <> id))
   pure unit
