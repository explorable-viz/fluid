module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried as FU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Prim.Row as Row
import Toppokki as T

launchFirefox
   :: forall options trash
    . Row.Union options trash T.LaunchOptions
   => { | options }
   -> Aff T.Browser
launchFirefox = T.runPromiseAffE1 _launchFirefox

foreign import _launchFirefox :: forall options. FU.Fn1 options (Effect (Promise T.Browser))

main :: Effect (Promise Unit)
main = fromAff (tests (launchFirefox {}))

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

   pure unit

   T.close browser

checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector)
   pure unit
