module Website.Test.FluidOrg where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Puppeteer (testURL)
import Toppokki as T

main :: Effect (Promise Unit)
main = fromAff $ sequence_ $ testURL ""
   [ testFig
   ]

testFig :: T.Page -> Aff Unit
testFig _ = pure unit
