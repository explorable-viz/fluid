module Test.Puppeteer where

import Prelude
import Data.String as String
import Effect (Effect)
import Test.Assert as Assert

-- Function to check if a string is non-empty
isNonEmpty :: String -> Boolean
isNonEmpty str = String.length str > 0

-- Test cases
main :: Effect Unit
main = do
   -- Test case for non-empty string
   let nonEmptyContent = "Hello, PureScript!"
   Assert.assert' "Content should be non-empty" (isNonEmpty nonEmptyContent)

   -- Test case for empty string
   let emptyContent = ""
   Assert.assert' "Content should be empty" (not (isNonEmpty emptyContent))


