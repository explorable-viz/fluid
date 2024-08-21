module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Foldable (sequence_)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Test.Util (TestSuite)
import Toppokki as T
import Util ((×))

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff $ sequence_ (snd <$> tests)

tests :: TestSuite
tests =
   [ "firefox-tests" × browserTests (launchFirefox)
   , "chrome-tests" × browserTests (T.launch {})
   ]

browserTests :: Aff T.Browser -> Aff Unit
browserTests launchBrowser = do
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080"
   log ("Going to " <> url)
   T.goto (T.URL url) page
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