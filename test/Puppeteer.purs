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

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

show' :: T.Selector -> String
show' (T.Selector sel) = sel

waitFor :: T.Selector -> T.Page -> Aff Unit
waitFor selector page = do
   log' ("Waiting for " <> show' selector)
   void $ T.pageWaitForSelector selector { timeout: 60000, visible: true } page
   log' "-> found"

puppeteerLogging :: Boolean
puppeteerLogging = true

-- Ignore Util.debug.logging flag for now
log' :: forall m. MonadEffect m => String -> m Unit
log' msg = when puppeteerLogging (log msg)

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff $ sequence_ tests

tests :: Array (Aff Unit)
tests =
   [ browserTests "chrome" (T.launch {})
   , browserTests "firefox" (launchFirefox)
   ]

browserTests :: String -> Aff T.Browser -> Aff Unit
browserTests browserName launchBrowser = do
   log ("browserTests: " <> browserName)
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080"
   log' ("Going to " <> url)
   T.goto (T.URL url) page
   --   log' "Waiting for content"
   --   log =<< content page
   checkFig4 page
   checkFig1 page
   checkFigConv2 page
   T.close browser

checkFig4 :: T.Page -> Aff Unit
checkFig4 page = do
   waitForFigure page (fig <> "-output")
   clickToggle page (fig <> "-input")
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
      testCondition fig (className == expectedClass && radius == "3.2") "circle-class-and-radius"
      let caption = T.Selector ("table#" <> fig <> "-input-renewables > caption.table-caption")
      checkTextContent fig page caption "renewables (4 of 240)"

checkFig1 :: T.Page -> Aff Unit
checkFig1 page = do
   waitForFigure page (fig <> "-bar-chart")
   waitForFigure page (fig <> "-line-chart")
   clickToggle page (fig <> "-input")
   clickBarChart
   where
   fig = "fig-1"

   clickBarChart :: Aff Unit
   clickBarChart = do
      let selector = T.Selector ("svg#" <> fig <> "-bar-chart rect.bar")
      waitFor selector page
      void $ T.click selector page
      fill <- getAttributeValue page selector "fill"
      testCondition fig (fill == "#57a157") "click-bar"

checkFigConv2 :: T.Page -> Aff Unit
checkFigConv2 page = do
   let fig = "fig-conv-2"
   waitForFigure page (fig <> "-output")
   clickToggle page (fig <> "-input")

waitForFigure :: T.Page -> String -> Aff Unit
waitForFigure page id = do
   let selector = T.Selector ("svg#" <> id)
   waitFor selector page

clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = T.Selector ("div#" <> id <> " + div > div > span.toggle-button")
   waitFor selector page
   log' ("Clicking " <> show' selector)
   void $ T.click selector page
   waitFor (T.Selector ("div#" <> id)) page

checkTextContent :: String -> T.Page -> T.Selector -> String -> Aff Unit
checkTextContent fig page selector expected = do
   waitFor selector page
   captionText <- textContentValue page selector
   testCondition fig (captionText == expected) "table-view-caption"
   pure unit

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   pure (unsafeFromForeign captionText)
