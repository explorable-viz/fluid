module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser (runParser)
import Bindings (Bindings(..), Var, (:+:), (↦))
import Desugar (Expr) as S
import Eval (defs, eval)
import Expr (Expr)
import Lattice (𝔹)
import Parse (module_, program, program2)
import Primitive (primitives)
import Util (type (×), (×), error, successful)
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

loadModule :: String -> Env 𝔹 -> Aff (Env 𝔹)
loadModule file ρ = do
   src <- loadFile "fluid/lib" file
   pure $ successful $ defs ρ $ successfulParse src module_

openWithImports :: String -> Aff (Env 𝔹 × Expr 𝔹)
openWithImports file =
   loadFile "fluid/example" file >>= parseWithImports

openWithImports2 :: String -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithImports2 file =
   loadFile "fluid/example" file >>= parseWithImports2

successfulParse :: forall t . String -> SParser t -> t
successfulParse src p =
   case runParser src p of
      Left parseError -> error $ show parseError
      Right t -> t

parseWithImports :: String -> Aff (Env 𝔹 × Expr 𝔹)
parseWithImports src = do
   (×) <$> (loadModule "prelude" primitives >>= loadModule "graphics")
       <@> successfulParse src program

parseWithImports2 :: String -> Aff (Env 𝔹 × S.Expr 𝔹)
parseWithImports2 src = do
   (×) <$> (loadModule "prelude" primitives >>= loadModule "graphics")
       <@> successfulParse src program2

openDatasetAs :: String -> Var -> Aff (Env 𝔹)
openDatasetAs file x = do
   ρ × e <- loadFile "fluid/dataset" file >>= parseWithImports
   let _ × v = successful $ eval ρ e
   pure $ Empty :+: x ↦ v
