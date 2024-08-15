module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.String (length)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Test.Toppokki as T
import Util (check)

main :: Effect (Promise Unit)
main = fromAff tests

tests :: Aff Unit
tests = do
   browser <- T.launch {}
   page <- T.newPage browser

   log "Waiting for 'goto' load"
   T.goto (T.URL "http://127.0.0.1:8080") {} page
   content <- T.content page
   log content
   check (length content > 0) "Content is non-empty string"
   checkForFigure page "fig-4-output"
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   checkForFigure page "fig-conv-2-output"
   T.close browser

checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector)
   pure unit

