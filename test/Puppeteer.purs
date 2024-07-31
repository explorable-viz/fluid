module Test.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Toppokki as T

main :: Effect (Promise (Unit))
main = Promise.fromAff tests

tests :: Aff Unit
tests = do
   browser <- T.launch {}
   page <- T.newPage browser
   T.goto (T.URL "http://127.0.0.1:8080") page
   content <- T.content page
   liftEffect (log content)
   T.close browser
   liftEffect (log "In Puppeteer.purs")

{-
main :: Effect Unit
main = launchAff_ do
   browser <- T.launch {}
   page <- T.newPage browser
   T.goto (T.URL "http://127.0.0.1:8080") page
   content <- T.content page
   liftEffect (log content)
   T.close browser
   liftEffect (log "In Puppeteer.purs")
-}
