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
resourceServerUrl = "."

loadFile :: String -> String -> Aff String
loadFile folder file = do
   let fileUrl = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   let request_ = defaultRequest { url = fileUrl, method = Left GET, responseFormat = string }
   trace request_ \_ -> do
      result <- request request_
      case result of
         Left err -> error $ printError err
         Right response ->
            trace response \_ ->
            pure response.body
