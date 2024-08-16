module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Toppokki as T

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff do
   tests (launchFirefox)
   tests (T.launch {})

tests :: Aff T.Browser -> Aff Unit
tests launchBrowser = do
   browser <- launchBrowser
   page <- T.newPage browser
   log "Waiting for 'goto' load"
   T.goto (T.URL "http://127.0.0.1:8080") page
   content <- T.content page
   log content
   checkForFigure page "fig-4-output"
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   checkForFigure page "fig-conv-2-output"

   let selector = "#fig-1-input + div > div > span.toggle-button"
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   _ <- T.click (T.Selector selector) page
   log ("Found " <> selector)

   pure unit

   T.close browser

checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector)
   pure unit
