module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser (runParser)
import Bindings (Bindings(..))
import Eval (defs)
import Expr (Expr)
import Parse (SParser, module_, program)
import Val (Env)
import Util (error)

-- For Wrattler integration. Should not end in "/".
resourceServerUrl :: String
resourceServerUrl = "."

loadFile :: String -> String -> Aff String
loadFile folder file = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error $ printError err
      Right response -> pure response.body

loadModule :: String -> Env -> Aff Env
loadModule file ρ = do
   src <- loadFile "fluid/lib" file
   let m = successfulParse src module_
   pure $ defs ρ m

openWithImports :: String -> Aff (Tuple Env Expr)
openWithImports file =
   loadFile "fluid/example" file >>= parseWithImports

successfulParse :: forall t . String -> SParser t -> t
successfulParse src p =
   case runParser src p of
      Left parseError -> error $ show parseError
      Right t -> t

parseWithImports :: String -> Aff (Tuple Env Expr)
parseWithImports src = do
   ρ' <- loadModule "prelude" Empty >>= loadModule "graphics"
   pure $ Tuple ρ' (successfulParse src program)
