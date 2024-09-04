module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (testCondition)
import Toppokki as T
import Util (debug)

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

show' :: T.Selector -> String
show' (T.Selector sel) = sel

waitFor :: T.Selector -> T.Page -> Aff Unit
waitFor selector page = do
   log' ("Waiting for " <> show' selector)
   void $ T.pageWaitForSelector selector { timeout: 60000, visible: true } page
   log' "-> found"

waitForHidden :: T.Selector -> T.Page -> Aff Unit
waitForHidden selector page = do
   log' ("Waiting for " <> show' selector)
   void $ T.pageWaitForSelector selector { timeout: 60000, visible: false } page
   log' "-> found"

puppeteerLogging :: Boolean
puppeteerLogging = false

log' :: forall m. MonadEffect m => String -> m Unit
log' msg = when (debug.logging || puppeteerLogging) (log msg)

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff $ sequence_ tests

tests :: Array (Aff Unit)
tests =
   [ browserTests "chrome" (T.launch {})
   , browserTests "firefox" (launchFirefox)
   ]

goto :: T.URL -> T.Page -> Aff Unit
goto (T.URL url) page = do
   log' ("Going to " <> show url)
   T.goto (T.URL url) page

-- Test each fig on a fresh page, else earlier tests seem to interfere with element visibility (on Firefox)
browserTests :: String -> Aff T.Browser -> Aff Unit
browserTests browserName launchBrowser = do
   log ("browserTests: " <> browserName)
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080"
   goto (T.URL url) page
   testScatterPlot page
   goto (T.URL url) page
   testBarChartLineChart page
   goto (T.URL url) page
   testConvolution page
   T.close browser

testScatterPlot :: T.Page -> Aff Unit
testScatterPlot page = do
   waitForFigure page (fig <> "-output")
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
   clickScatterPlotPoint

   where
   fig = "fig-4"

   clickScatterPlotPoint :: Aff Unit
   clickScatterPlotPoint = do
      let selector = T.Selector ("div#" <> fig <> " .scatterplot-point")
      waitFor selector page
      void $ T.click selector page
      className <- getAttributeValue page selector "class"
      radius <- getAttributeValue page selector "r"
      let expectedClass = "scatterplot-point selected-primary-persistent selected-primary-transient"
      testCondition (show' selector) (className == expectedClass && radius == "3.2") "class and radius"
      let caption = T.Selector ("table#" <> fig <> "-input-renewables > caption.table-caption")
      checkTextContent page caption "renewables (4 of 240)"

testBarChartLineChart :: T.Page -> Aff Unit
testBarChartLineChart page = do
   waitForFigure page (fig <> "-bar-chart")
   waitForFigure page (fig <> "-line-chart")
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
   clickBarChart
   where
   fig = "fig-1"

   clickBarChart :: Aff Unit
   clickBarChart = do
      let selector = T.Selector ("svg#" <> fig <> "-bar-chart rect.bar")
      waitFor selector page
      void $ T.click selector page
      fill <- getAttributeValue page selector "fill"
      testCondition (show' selector) (fill == "#57a157") "fill"

testConvolution :: T.Page -> Aff Unit
testConvolution page = do
   let fig = "fig-conv-2"
   waitForFigure page (fig <> "-output")
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page

waitForFigure :: T.Page -> String -> Aff Unit
waitForFigure page id = do
   let selector = T.Selector ("svg#" <> id)
   waitFor selector page
   testCondition (show' selector) true "exists"

clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = T.Selector ("div#" <> id <> " + div > div > span.toggle-button")
   waitFor selector page
   log' ("Clicking " <> show' selector)
   void $ T.click selector page

checkTextContent :: T.Page -> T.Selector -> String -> Aff Unit
checkTextContent page selector expected = do
   waitFor selector page
   captionText <- textContentValue page selector
   testCondition (show' selector) (captionText == expected) "text content"
   pure unit

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   pure (unsafeFromForeign captionText)
