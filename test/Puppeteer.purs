module Test.Puppeteer where

import Prelude 
import Toppokki as T
import Effect (Effect)
import Effect.Aff (launchAff_)
--import Effect.Aff (Aff)
--import Data.String as String
--import Test.Assert as Assert

main :: Effect Unit
main = launchAff_ do
   browser <- T.launch {}
   page <- T.newPage browser
   T.goto (T.URL "https://example.com") page
   --content <- T.content page
   --Assert.assert' "content is non-empty string" (String.length content > 0)
   pure unit

{-
main :: Effect Unit
main = launchAff_ do
   browser <- T.launch {}
   page <- T.newPage browser
   _ <- T.screenshot {path: "./test/test.png"} page
   _ <- T.pdf {path: "./test/test.pdf"} page
   pure unit
   T.close browser
-}

{-
puppeteer :: Array BenchSuite
   [-- check circle position function
    -- check text position function
    -- etc
   ]
-}

