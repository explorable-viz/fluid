module Test.Puppeteer where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Assert as Assert
import Toppokki as T
--import Data.String as String

-- Function to check if a string is non-empty
isNotEmpty :: String -> Boolean
isNotEmpty str = String.length str > 0

main :: Effect Unit
main = do

   -- Test case for non-empty string
   let nonEmptyContent = "Hello, PureScript!"
   Assert.assert' "Content should be non-empty" (isNotEmpty nonEmptyContent)

   -- Test case for empty string
   let emptyContent = ""
   Assert.assert' "Content should be empty" (not (isNotEmpty emptyContent))

   ------------------
   launchAff_ do
      browser <- T.launch {}
      page <- T.newPage browser
      T.goto (T.URL "http://127.0.0.1:8080") page
      content <- T.content page
      liftEffect (Assert.assert' "Content is non-empty string" (String.length content > 0))
      liftEffect (log content)
      T.close browser
   log "hello"
