module Standalone.Test.Convolution where

import Prelude hiding (absurd)

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Test.Util.Puppeteer (clickToggle, goto, launchFirefox, waitFor, waitForFigure)
import Toppokki as T

tests :: Array (Aff Unit)
tests =
   [ browserTests "chrome" (T.launch {})
   , browserTests "firefox" (launchFirefox)
   ]

-- Test each fig on a fresh page, else earlier tests seem to interfere with element visibility (on Firefox)
browserTests :: String -> Aff T.Browser -> Aff Unit
browserTests browserName launchBrowser = do
   log ("browserTests: " <> browserName)
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080/convolution"
   goto (T.URL url) page
   testFig page
   T.close browser

testFig :: T.Page -> Aff Unit
testFig page = do
   let figId = "fig"
   waitForFigure page (figId <> "-output")
   let toggle = figId <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page

main :: Effect (Promise Unit)
main = fromAff $ sequence_ tests
