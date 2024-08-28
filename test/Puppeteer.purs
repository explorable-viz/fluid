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
   checkForFigure page "fig-4-output"
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   checkForFigure page "fig-conv-2-output"

   clickToggle page "fig-4-input"
   clickToggle page "fig-1-input"
   clickToggle page "fig-conv-2-input"

   clickScatterPlotPoint page "fig-4"
   --checkLineChartPlotPoints page

   --clickBarChart page
   let selector = "svg#fig-1-bar-chart rect.bar"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found BAR CHART " <> selector)
   _ <- T.click (T.Selector selector) page
   log ("Clicked on " <> selector)

   --_ <- T.pageWaitForSelector (T.Selector "svg#fig-1-bar-chart") { fill= "57a157" } page
   fill <- getAttributeValue page (T.Selector "svg#fig-1-bar-chart .bar") "fill"
   log ("Fill colour is: " <> fill)
   if fill == "#57a157" then log "The first bar in bar chart has been clicked."
   else log "The first bar in bar chart has not been successfully clicked."

   T.close browser

--Function to check for the presence of an SVG figure
checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector)
   pure unit

--Function to click a toggle
clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = "div#" <> id <> " + div > div > span.toggle-button"
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector <> ", clicking the toggle button")
   _ <- T.click (T.Selector selector) page
   log ("Clicked on " <> selector)
   _ <- T.pageWaitForSelector (T.Selector ("div#" <> id)) { visible: true } page
   pure unit

clickScatterPlotPoint :: T.Page -> String -> Aff Unit
clickScatterPlotPoint page id = do
   let selector = "div#" <> id <> " .scatterplot-point"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   _ <- T.click (T.Selector selector) page
   log ("Clicked on " <> selector)
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
