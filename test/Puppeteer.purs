module Test.Puppeteer where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Foldable (sequence_)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Test.Util (TestSuite)
import Toppokki as T
import Util (Endo, check, debug, (×))

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff $ sequence_ (snd <$> tests)

tests :: TestSuite
tests =
   [ "firefox-tests" × browserTests (launchFirefox)
   , "chrome-tests" × browserTests (T.launch {})
   ]

browserTests :: Aff T.Browser -> Aff Unit
browserTests launchBrowser = do
   browser <- launchBrowser
   page <- T.newPage browser
   let url = "http://127.0.0.1:8080"
   log' ("Going to " <> url)
   T.goto (T.URL url) page
   content <- T.content page
   log' content

   checkFig4 page
   checkFig1 page
   checkFigConv2 page

   T.close browser

----------------------
checkFig4 :: T.Page -> Aff Unit
checkFig4 page = do
   checkForFigure page "fig-4-output"
   clickToggle page "fig-4-input"
   clickScatterPlotPoint page "fig-4"
   log' "checkFig4 completed"

checkFig1 :: T.Page -> Aff Unit
checkFig1 page = do
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   clickToggle page "fig-1-input"
   clickBarChart "fig-1" page "fig-1-bar-chart"
   log' "checkFig1 completed"

checkFigConv2 :: T.Page -> Aff Unit
checkFigConv2 page = do
   checkForFigure page "fig-conv-2-output"
   clickToggle page "fig-conv-2-input"
   log' "checkFigConv2 completed"

----------------------
--Function to check for the presence of an SVG figure
checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   pure unit

--Function to click a toggle
clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = T.Selector ("div#" <> id <> " + div > div > span.toggle-button")
   _ <- T.pageWaitForSelector selector { timeout: 60000 } page
   _ <- T.click selector page
   _ <- T.pageWaitForSelector (T.Selector ("div#" <> id)) { visible: true } page
   pure unit

clickScatterPlotPoint :: T.Page -> String -> Aff Unit
clickScatterPlotPoint page id = do
   let selector = T.Selector ("div#" <> id <> " .scatterplot-point")
   _ <- T.pageWaitForSelector selector { timeout: 60000, visible: true } page
   _ <- T.click selector page
   className <- getAttributeValue page selector "class"
   radius <- getAttributeValue page selector "r"
   check' id (className == "scatterplot-point selected-primary-persistent selected-primary-transient" && radius == "3.2") "circle class and radius"
   checkCaptionText id page "table#fig-4-input-renewables > caption.table-caption"

checkCaptionText :: String -> T.Page -> String -> Aff Unit
checkCaptionText fig page selector = do
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   captionText <- textContentValue page (T.Selector selector)
   check' fig (captionText == "renewables (4 of 240)") "caption (4 of 240)"
   pure unit

clickBarChart :: String -> T.Page -> String -> Aff Unit
clickBarChart fig page id = do
   let selector = T.Selector ("svg#" <> id <> " rect.bar")
   _ <- T.pageWaitForSelector selector { timeout: 60000 } page
   _ <- T.click selector page
   fill <- getAttributeValue page selector "fill"
   check' fig (fill == "#57a157") "first bar clicked"

-------------

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)

textContentValue :: T.Page -> T.Selector -> Aff String
textContentValue page selector = do
   captionText <- T.unsafePageEval selector "element => element.textContent" page
   pure (unsafeFromForeign captionText)

-------------------

report :: Boolean -> Endo String
report b s = "\x1b[" <> (if b then "32" else "31") <> "m " <> (if b then "✔" else "✖") <> "\x1b[0m " <> s

check' :: forall m. MonadThrow Error m => MonadEffect m => String -> Boolean -> String -> m Unit
check' fig b s = check'' b (fig <> "/" <> s)
   where
   check'' true s' = do
      log (report true s')
      pure unit
   check'' false s' = check false (report false s')

log' :: forall m. MonadEffect m => String -> m Unit
log' message =
   when debug.logging (log message)
