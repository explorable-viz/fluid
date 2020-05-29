module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.List (List(..), (:), concat)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser (runParser)
import Bindings (Bindings(..))
import Expr (Expr)
import Parse (SParser, program)
import Val (Env(..))
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

loadModule :: Env -> String -> Aff Env
loadModule ρ file = do
   -- assumed to be sequence of lets, which we make into an expression
   src <- loadFile "fluid/lib" (file <> "0")
   let e = successfulParse src program
   pure Empty -- todo
--   case e of
--      E.Defs -> return Eval.defs(ρ, e.def̅, emptyEnv())[1]

module_prelude :: Env
module_prelude = loadModule Empty "prelude"

module_graphics :: Env
module_graphics = loadModule module_prelude "graphics"

openWithImports :: String -> List Env -> Tuple Env Expr
openWithImports file = parseWithImports (loadFile "fluid/example" file)

successfulParse :: forall t . String -> SParser t -> t
successfulParse src p =
   case runParser src p of
      Left parseError -> error $ show parseError
      Right t -> t

parseWithImports :: String -> List Env -> Tuple Env Expr
parseWithImports src modules =
   Tuple (concat (module_prelude : module_graphics : modules)) (successfulParse src)

