module Website.Test.FluidOrg.Convolution where

import Prelude hiding (absurd)

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Puppeteer (clickToggle, testURL, waitFor, waitForFigure)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ $ testURL "convolution"
   [ testFig
   ]

testFig :: T.Page -> Aff Unit
testFig page = do
   let figId = "fig"
   waitForFigure page (figId <> "-output")
   let toggle = figId <> "-input"
   clickToggle page toggle
   waitFor (T.Selector ("div#" <> toggle)) page
