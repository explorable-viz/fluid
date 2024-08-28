module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Foldable (sequence_)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (TestSuite)
import Toppokki as T
import Util ((×))

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

foreign import _launchFirefox :: Effect (Promise T.Browser)

--foreign import checkLineChartPlotPoints :: T.Page -> Aff Unit

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

   checkFig4 page
   checkFig1 page
   checkFigConv2 page

   T.close browser

----------------------
checkFig4 :: T.Page -> Aff Unit
checkFig4 page = do
   checkForFigure page "fig-4-output"
   clickToggle page "fig-4-input"
   clickScatterPlotPoint page "fig-4"
   log "checkFig4 completed"

checkFig1 :: T.Page -> Aff Unit
checkFig1 page = do
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   clickToggle page "fig-1-input"
   clickBarChart page "fig-1-bar-chart"
   log "checkFig1 completed"

checkFigConv2 :: T.Page -> Aff Unit
checkFigConv2 page = do
   checkForFigure page "fig-conv-2-output"
   clickToggle page "fig-conv-2-input"
   log "checkFigConv2 completed"

----------------------
--Function to check for the presence of an SVG figure
checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   --log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   --log ("Found " <> selector)
   pure unit

--Function to click a toggle
clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = "div#" <> id <> " + div > div > span.toggle-button"
   --log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   --log ("Found " <> selector <> ", clicking the toggle button")
   _ <- T.click (T.Selector selector) page
   --log ("Clicked on " <> selector)
   _ <- T.pageWaitForSelector (T.Selector ("div#" <> id)) { visible: true } page
   pure unit

clickScatterPlotPoint :: T.Page -> String -> Aff Unit
clickScatterPlotPoint page id = do
   let selector = "div#" <> id <> " .scatterplot-point"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   _ <- T.click (T.Selector selector) page
   --log ("Clicked on " <> selector)
   className <- getAttributeValue page (T.Selector selector) "class"
   radius <- getAttributeValue page (T.Selector selector) "r"
   if className == "scatterplot-point selected-primary-persistent selected-primary-transient" && radius == "3.2" then log "The circle's class and radius have changed as expected."
   else log "The circle's class and/or radius did not change as expected."
   checkCaptionText page "table#fig-4-input-renewables > caption.table-caption"

checkCaptionText :: T.Page -> String -> Aff Unit
checkCaptionText page selector = do
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   captionText <- textContentValue page (T.Selector selector)
   if captionText == "renewables (4 of 240)" then log "The caption contains the expected value."
   else log "The caption does not contain the expected value."
   pure unit

clickBarChart :: T.Page -> String -> Aff Unit
clickBarChart page id = do
   let selector = "svg#" <> id <> " rect.bar"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   --log ("Found BAR CHART " <> selector)
   _ <- T.click (T.Selector selector) page
   --log ("Clicked on " <> selector)
   fill <- getAttributeValue page (T.Selector selector) "fill"
   --log ("Fill colour is: " <> fill)
   if fill == "#57a157" then log "The first bar in bar chart has been clicked."
   else log "The first bar in bar chart has not been successfully clicked."

--check ((fill == "#57a157") (log "The first bar in bar chart has been clicked.") (log "The first bar in bar chart has not been successfully clicked."))

-------------

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   --log ("captionText " <> text)
   pure (unsafeFromForeign captionText)
