module Test.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Assert as Assert
import Toppokki as T

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
   checkForFigure page "fig-4"
   checkForFigure page "fig-1"
   checkForFigure page "fig-conv-2"
   T.close browser
   liftEffect (log "In Puppeteer.purs")

checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = T.Selector ("div#" <> id)
   let options = { visible: true }
   _ <- T.pageWaitForSelector selector options page
   pure unit
