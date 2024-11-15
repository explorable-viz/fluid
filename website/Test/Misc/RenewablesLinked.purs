module Website.Test.Misc.RenewablesLinked where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Puppeteer (checkAttribute, click, clickToggle, testURL, waitFor, waitForFigure)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ $ testURL "renewables-linked"
   [ testFig
   ]

testFig :: T.Page -> Aff Unit
testFig page = do
   waitForFigure page barChart
   waitForFigure page lineChart
   checkXTicks
   checkPointRadius

   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
   clickBarChart
   where
   fig = "fig"
   barChart = fig <> "-barChart"
   lineChart = fig <> "-lineChart"

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
