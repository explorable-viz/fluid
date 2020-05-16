module Module where

import Prelude
import Effect (Effect)
import Effect.Console (log)


-- Placeholder for Wrattler integration. Should not end in "/", use "." for local run
resourceServerUrl :: String
resourceServerUrl = "."

--
loadFile :: String -> String -> Effect String
loadFile folder file = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   log $ "Opening" <> url
   pure "hello"
{-
   xmlhttp.open("GET", url, false)
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
