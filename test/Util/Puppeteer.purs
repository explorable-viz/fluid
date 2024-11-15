module Test.Util.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Data.Foldable (for_)
import Data.Function.Uncurried as FU
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (testCondition)
import Toppokki (runPromiseAffE1)
import Toppokki as T
import Util (debug)

puppeteerTests
   :: { logging :: Boolean
      , headless :: Boolean
      }
puppeteerTests =
   { logging: true
   , headless: true
   }

foreign import _launch :: forall options. FU.Fn1 options (Effect (Promise T.Browser))

launch
   :: { browser :: String
      , defaultViewport :: Record T.DefaultViewPort
      , headless :: Boolean
      }
   -> Aff T.Browser
launch = runPromiseAffE1 _launch

browserTests :: String -> String -> Aff T.Browser -> Array (T.Page -> Aff Unit) -> Aff Unit
browserTests suffix browserName launchBrowser tests = do
   log ("browserTests: " <> browserName)
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080/" <> suffix
   -- Test each fig on fresh page, else earlier tests seem to interfere with element visibility (on Firefox)
   for_ tests $ \test -> do
      goto (T.URL url) page
      test page
   T.close browser

defaultViewport :: Record T.DefaultViewPort
defaultViewport =
   { deviceScaleFactor: 1.0
   , hasTouch: false
   , height: 800.0
   , isLandscape: false
   , isMobile: false
   , width: 1200.0
   }

testURL :: String -> Array (T.Page -> Aff Unit) -> Array (Aff Unit)
testURL suffix tests =
   [ testOn "chrome", testOn "firefox" ]
   where
   testOn :: String -> Aff Unit
   testOn browser = browserTests suffix browser launchBrowser tests
      where
      launchBrowser :: Aff T.Browser
      launchBrowser = launch { browser, defaultViewport, headless: puppeteerTests.headless }

show' :: T.Selector -> String
show' (T.Selector sel) = sel

timeout :: Int
timeout = 240000

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

log' :: forall m. MonadEffect m => String -> m Unit
log' = log >>> when (debug.logging || puppeteerTests.logging)

goto :: T.URL -> T.Page -> Aff Unit
goto (T.URL url) page = do
   log ("Going to " <> show url)
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

waitForFigure :: T.Page -> String -> Aff Unit
waitForFigure page id =
   waitFor (T.Selector ("svg#" <> id)) page

clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let toggle = T.Selector ("div#" <> id <> " + div .toggle-button")
   waitFor toggle page
   click toggle page
