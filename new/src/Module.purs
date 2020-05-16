module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Console (log)


-- Placeholder for Wrattler integration. Should not end in "/", use "." for local run
resourceServerUrl :: String
resourceServerUrl = "."

--
loadFile :: String -> String -> Effect String
loadFile folder file = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   log $ "Opening" <> url
   _ <- launchAff $ do
      result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
      case result of
         Left err -> do
            pure $ printError err
         Right response -> pure response.body
   pure "hello"
{-
   xmlhttp.send()
   if (xmlhttp.status === 200) {
      text = xmlhttp.responseText
   }
   if (text! === undefined) {
      return assert(false, `${url} could not be loaded.`)
   } else  {
      return text!
   }
-}
