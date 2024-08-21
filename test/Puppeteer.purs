module Test.Puppeteer where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Foreign (unsafeFromForeign)
import Toppokki as T

launchFirefox :: Aff T.Browser
launchFirefox = toAffE _launchFirefox

foreign import _launchFirefox :: Effect (Promise T.Browser)

main :: Effect (Promise Unit)
main = fromAff do
   tests (launchFirefox)
   tests (T.launch {})

tests :: Aff T.Browser -> Aff Unit
tests launchBrowser = do
   browser <- launchBrowser
   page <- T.newPage browser
   log "Waiting for 'goto' load"
   T.goto (T.URL "http://127.0.0.1:8080") page
   content <- T.content page
   log content
   checkForFigure page "fig-4-output"
   checkForFigure page "fig-1-bar-chart"
   checkForFigure page "fig-1-line-chart"
   checkForFigure page "fig-conv-2-output"

   clickToggle page "fig-4-input"
   clickToggle page "fig-1-input"
   clickToggle page "fig-conv-2-input"

   clickScatterPlotPoint page "fig-4"
   --------------
   let selector = "table#fig-4-input-renewables > caption.table-caption"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   captionText <- T.unsafePageEval (T.Selector selector) "element => element.textContent" page
   log ("captionText " <> unsafeFromForeign captionText)

   T.close browser

--Function to check for the presence of an SVG figure
checkForFigure :: T.Page -> String -> Aff Unit
checkForFigure page id = do
   let selector = "svg#" <> id
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector)
   pure unit

--Function to click a toggle
clickToggle :: T.Page -> String -> Aff Unit
clickToggle page id = do
   let selector = "div#" <> id <> " + div > div > span.toggle-button"
   log ("Waiting for " <> selector)
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000 } page
   log ("Found " <> selector <> ", clicking the toggle button")
   _ <- T.click (T.Selector selector) page
   log ("Clicked on " <> selector)

   _ <- T.pageWaitForSelector (T.Selector ("div#" <> id)) { visible: true } page
   pure unit

clickScatterPlotPoint :: T.Page -> String -> Aff Unit
clickScatterPlotPoint page id = do
   let selector = "div#" <> id <> " .scatterplot-point"
   _ <- T.pageWaitForSelector (T.Selector selector) { timeout: 60000, visible: true } page
   _ <- T.click (T.Selector selector) page
   log ("Clicked on " <> selector)

   className <- getAttributeValue page (T.Selector selector) "class"
   radius <- getAttributeValue page (T.Selector selector) "r"

   if className == "scatterplot-point selected-primary-persistent selected-primary-transient" && radius == "3.2" then log "The circle's class and radius have changed as expected."
   else log "The circle's class and/or radius did not change as expected."

getAttributeValue :: T.Page -> T.Selector -> String -> Aff String
getAttributeValue page selector attribute = do
   attrValue <- T.unsafePageEval selector ("element => element.getAttribute('" <> attribute <> "')") page
   pure (unsafeFromForeign attrValue)
