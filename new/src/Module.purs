module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.List (List(..), (:), concat)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Expr (Expr)
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
{-
loadModule :: Env -> String -> Aff Env
loadModule ρ file =
   -- assumed to be sequence of lets, which we make into an expression
   let e = successfulParse (loadFile "fluid/lib" file <> "0") in
   error "todo"
--   case e of
--      E.Defs -> return Eval.defs(ρ, e.def̅, emptyEnv())[1]

openWithImports :: String -> List Env -> Tuple Env Expr
openWithImports file = parseWithImports (loadFile "fluid/example" file)

parseWithImports :: String -> List Env -> Tuple Env Expr
parseWithImports src modules =
   Tuple (concat (Module.prelude : Module.graphics : modules)) (successfulParse src)
-}
