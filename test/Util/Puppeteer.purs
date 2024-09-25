module Test.Util.Puppeteer where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (testCondition)
import Toppokki as T
import Util (debug)

foreign import _launchFirefox :: Effect (Promise T.Browser)

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

show' :: T.Selector -> String
show' (T.Selector sel) = sel

timeout :: Int
timeout = 60000

waitFor :: T.Selector -> T.Page -> Aff Unit
waitFor selector page = do
   log' ("Waiting for " <> show' selector)
   catchError
      ( do
           void $ T.pageWaitForSelector selector { timeout, visible: true } page
           log' "-> found"
           report true "exists"
      )
      \e ->
         report false (show e)
   where
   report = testCondition (show' selector)

waitForHidden :: T.Selector -> T.Page -> Aff Unit
waitForHidden selector page = do
   log' ("Waiting for " <> show' selector)
   void $ T.pageWaitForSelector selector { timeout, visible: false } page
   log' "-> found"

puppeteerLogging :: Boolean
puppeteerLogging = false

log' :: forall m. MonadEffect m => String -> m Unit
log' msg =
   when (debug.logging || puppeteerLogging)
      (log msg)

goto :: T.URL -> T.Page -> Aff Unit
goto (T.URL url) page = do
   log' ("Going to " <> show url)
   T.goto (T.URL url) page

click :: T.Selector -> T.Page -> Aff Unit
click element page = do
   T.click element page
   testCondition (show' element) true "click"

checkAttribute :: T.Page -> T.Selector -> String -> String -> Aff Unit
checkAttribute page sel attr expected = do
   found <- getAttributeValue page sel attr
   let errorMsg = if found == expected then "" else (" (got " <> found <> ")")
   testCondition (show' sel) (found == expected) (attr <> " == " <> show expected <> errorMsg)

checkAttributeContains :: T.Page -> T.Selector -> String -> String -> Aff Unit
checkAttributeContains page sel attr expected = do
   found <- getAttributeValue page sel attr
   let success = contains (Pattern expected) found
   let errorMsg = if success then "" else (" (got " <> found <> ")")
   testCondition (show' sel) success (attr <> " contains " <> show expected <> errorMsg)

checkTextContent :: T.Page -> T.Selector -> String -> Aff Unit
checkTextContent page selector expected = do
   waitFor selector page
   text <- textContentValue page selector
   testCondition (show' selector) (text == expected) ("text == " <> show expected)
   pure unit

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   pure (unsafeFromForeign captionText)
