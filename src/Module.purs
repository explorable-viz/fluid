module Module where

import Prelude
import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Bifunctor (bimap)
import Effect.Aff (Aff)
import Text.Parsing.Parser (runParser)
import Bindings (Var, (↦))
import SExpr (Expr) as S
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import Lattice (𝔹)
import Parse (module_, program)
import Primitive.Defs (primitives)
import Util (MayFail, type (×), (×), error, successful)
import Util.Parse (SParser)
import Util.SnocList (SnocList(..), (:-))
import Val (Env)

-- For Wrattler integration. Should not end in "/".
resourceServerUrl :: String
resourceServerUrl = "."

loadFile :: String -> String -> Aff String
loadFile folder file = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

parse :: forall t . String -> SParser t -> MayFail t
parse src = runParser src >>> show `bimap` identity

loadModule :: String -> Env 𝔹 -> Aff (Env 𝔹)
loadModule file ρ = do
   src <- loadFile "fluid/lib" file
   pure (successful (parse src module_ >>= desugarModuleFwd >>= eval_module ρ))

parseProgram :: String -> String -> Aff (S.Expr 𝔹)
parseProgram folder file = loadFile folder file <#> (successful <<< flip parse program)

open :: String -> Aff (S.Expr 𝔹)
open = parseProgram "fluid/example"

defaultImports :: Aff (Env 𝔹)
defaultImports =
   loadModule "prelude" primitives >>= loadModule "graphics" >>= loadModule "convolution"

openWithDefaultImports :: String -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithDefaultImports file = do
   ρ <- defaultImports
   open file <#> (ρ × _)

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: String -> Var -> Aff (Env 𝔹 × Env 𝔹)
openDatasetAs file x = do
   s <- parseProgram "fluid" file
   ρ <- defaultImports
   let _ × v = successful (desugarFwd s >>= eval ρ)
   pure (ρ × (Lin :- x ↦ v))
