module Test.Puppeteer where

import Prelude
import Toppokki as T
import Effect (Effect)
import Effect.Aff (launchAff_)

--import Test.Util.Mocha (run)

main :: Effect Unit
main = launchAff_ do
   browser <- T.launch {}
   --T.goto (T.URL "http://127.0.0.1:8080") page
   T.close browser

{-
puppeteer :: Array BenchSuite
   [-- check circle position function
    -- check text position function
    -- etc
   ]
-}

