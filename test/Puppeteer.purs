module Test.Puppeteer where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Toppokki as T

main :: Effect Unit
main = do
   launchAff_ do
      browser <- T.launch {}
      page <- T.newPage browser
      T.goto (T.URL "http://127.0.0.1:8080") page
      content <- T.content page
      --liftEffect (Assert.assert' "Content is non-empty string" (String.length content > 0))
      liftEffect (log content)
      --liftEffect (Assert.assertTrue' "Graph exists" (String.contains (String.Pattern "fig-4") content))
      T.close browser
   log "In Puppeteer.purs"
