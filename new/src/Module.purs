module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Debug.Trace (trace)
import Effect.Aff (Aff)
import Util (error)


-- Placeholder for Wrattler integration. Should not end in "/", use "." for local run
resourceServerUrl :: String
resourceServerUrl = "http://127.0.0.1:1234"

loadFile :: String -> String -> Aff String
loadFile folder file = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   let request_ = defaultRequest { url = url, method = Left GET, responseFormat = string }
   trace request_ \_ -> do
      result <- request request_
      case result of
         Left err -> error $ printError err
         Right response ->
            trace response \_ ->
            pure response.body
