module Test.Puppeteer where

import Prelude
import Toppokki as T
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ do
   browser <- T.launch {}
   T.close browser