module Website.Test.Misc.RenewablesLinked where

import Prelude

import App.View.Util.D3 (nthChildOf)
import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Puppeteer (checkAttribute, click, clickToggle, testURL, waitFor)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ $ testURL "renewables-linked"
   [ testFig
   ]

testFig :: T.Page -> Aff Unit
testFig page = do
   waitFor (T.Selector barChart) page
   waitFor (T.Selector lineChart) page
   checkXTicks
   checkPointRadius

   clickToggle page "fig-data-pane"
   clickBarChart
   where
   barChart = nthChildOf "#fig-output" 1
   lineChart = nthChildOf "#fig-output" 2

   clickBarChart :: Aff Unit
   clickBarChart = do
      let bar = T.Selector (barChart <> " rect.bar")
      waitFor bar page
      click bar page
      checkAttribute page bar "fill" "#57a157"

   checkXTicks :: Aff Unit
   checkXTicks =
      waitFor (T.Selector (lineChart <> " g.x-axis")) page

   checkPointRadius :: Aff Unit
   checkPointRadius = do
      let point = T.Selector (lineChart <> " circle.linechart-point")
      waitFor point page
      checkAttribute page point "r" "2.0"
