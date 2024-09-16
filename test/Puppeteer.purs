module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Test.Util.Puppeteer (checkAttribute, checkAttributeContains, checkTextContent, click, goto, launchFirefox, waitFor)
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
      checkAttributeContains page point "class" "selected-primary-persistent"
      checkAttribute page point "r" "3.2"
      let caption = T.Selector ("table#" <> fig <> "-input-renewables > caption.table-caption")
      checkTextContent page caption "renewables (40 of 240)"

testBarChartLineChart :: T.Page -> Aff Unit
testBarChartLineChart page = do
   waitForFigure page barChart
   waitForFigure page lineChart
   checkXTicks
   checkPointRadius

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
      checkAttribute page bar "fill" "#57a157"

   checkXTicks :: Aff Unit
   checkXTicks =
      waitFor (T.Selector ("svg#" <> lineChart <> " g.x-axis")) page

   checkPointRadius :: Aff Unit
   checkPointRadius = do
      let point = T.Selector ("svg#" <> lineChart <> " circle.linechart-point")
      waitFor point page
      checkAttribute page point "r" "2.0"

testConvolution :: T.Page -> Aff Unit
testConvolution page = do
   let fig = "fig-conv-2"
   waitForFigure page (fig <> "-output")
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page

waitForFigure :: T.Page -> String -> Aff Unit
waitForFigure page id =
   waitFor (T.Selector ("svg#" <> id)) page

clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let toggle = T.Selector ("div#" <> id <> " + div > div > span.toggle-button")
   waitFor toggle page
   click toggle page
