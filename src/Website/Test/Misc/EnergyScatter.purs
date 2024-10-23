module Website.Test.Misc.EnergyScatter where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Puppeteer (checkAttribute, checkAttributeContains, checkTextContent, click, clickToggle, testURL, waitFor, waitForFigure)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ $ testURL "energy-scatter"
   [ testFig
   ]

testFig :: T.Page -> Aff Unit
testFig page = do
   waitForFigure page (fig <> "-output")
   let toggle = fig <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
   clickScatterPlotPoint

   where
   fig = "fig"

   clickScatterPlotPoint :: Aff Unit
   clickScatterPlotPoint = do
      let point = T.Selector ("div#" <> fig <> " .scatterplot-point")
      waitFor point page
      click point page
      checkAttributeContains page point "class" "selected-primary-persistent"
      checkAttribute page point "r" "3.2"
      let caption = T.Selector ("table#" <> fig <> "-input-renewables > caption.table-caption")
      checkTextContent page caption "renewables (40 of 240)"
