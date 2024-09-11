module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (testCondition)
import Test.Util.Puppeteer (click, goto, launchFirefox, show', waitFor)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ tests

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
      let point = T.Selector ("div#" <> fig <> " .scatterplot-point")
      waitFor point page
      click point page
      className <- getAttributeValue page point "class"
      let expectedClass = "selected-primary-persistent"
      testCondition (show' point) (contains (Pattern expectedClass) className) ("class contains " <> show expectedClass)
      radius <- getAttributeValue page point "r"
      let expectedRadius = "3.2"
      testCondition (show' point) (radius == expectedRadius) ("radius == " <> show expectedRadius)
      let caption = T.Selector ("table#" <> fig <> "-input-renewables > caption.table-caption")
      checkTextContent page caption "renewables (40 of 240)"

testBarChartLineChart :: T.Page -> Aff Unit
testBarChartLineChart page = do
   waitForFigure page barChart
   waitForFigure page lineChart
   checkXTicks
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
   clickBarChart
   where
   fig = "fig-1"
   barChart = fig <> "-bar-chart"
   lineChart = (fig <> "-line-chart")

   clickBarChart :: Aff Unit
   clickBarChart = do
      let bar = T.Selector ("svg#" <> barChart <> " rect.bar")
      waitFor bar page
      click bar page
      fill <- getAttributeValue page bar "fill"
      let expected = "#57a157"
      testCondition (show' bar) (fill == expected) ("fill == " <> show expected)

   checkXTicks :: Aff Unit
   checkXTicks = do
      waitFor (T.Selector ("svg#" <> lineChart <> " g.x-axis")) page

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

clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let toggle = T.Selector ("div#" <> id <> " + div > div > span.toggle-button")
   waitFor toggle page
   click toggle page

checkTextContent :: T.Page -> T.Selector -> String -> Aff Unit
checkTextContent page selector expected = do
   waitFor selector page
   text <- textContentValue page selector
   liftEffect $ log ("TESTING: " <> text)
   testCondition (show' selector) (text == expected) ("text == " <> show expected)
   pure unit

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   pure (unsafeFromForeign captionText)
