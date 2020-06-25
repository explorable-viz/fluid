module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser (runParser)
import Eval (defs)
import Expr (Expr)
import Parse (module_, program)
import Primitive (primitives)
import Util (type (×), (×), error)
import Util.Parse (SParser)
import Val (Env)

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
   case defs ρ $ successfulParse src module_ of
      Left msg -> error msg
      Right ρ' -> pure ρ'

openWithImports :: String -> Aff (Env × Expr)
openWithImports file =
   loadFile "fluid/example" file >>= parseWithImports

successfulParse :: forall t . String -> SParser t -> t
successfulParse src p =
   case runParser src p of
      Left parseError -> error $ show parseError
      Right t -> t

parseWithImports :: String -> Aff (Env × Expr)
parseWithImports src = do
   (×) <$> (loadModule "prelude" primitives >>= loadModule "graphics")
       <@> successfulParse src program
