module Module2 where

import Prelude
import Affjax.Web (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Bifunctor (bimap)
import Effect.Aff (Aff)
import Parsing (runParser)
import Bindings2 (Var, (↦))
import DesugarFwd2 (desugarFwd, desugarModuleFwd)
import Eval2 (eval, eval_module)
import Lattice2 (𝔹)
import Parse2 (module_, program)
import Primitive.Defs2 (primitives)
import SExpr2 (Expr) as S
import Util2 (MayFail, type (×), (×), error, successful)
import Util.Parse2 (SParser)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Env)

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive newtype instance showFile :: Show File
derive newtype instance semigroupFile :: Semigroup File
derive newtype instance monoidFile :: Monoid File

-- For Wrattler integration. Should not end in "/".
resourceServerUrl :: String
resourceServerUrl = "."

loadFile :: Folder -> File -> Aff String
loadFile (Folder folder) (File file) = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

parse :: forall t . String -> SParser t -> MayFail t
parse src = runParser src >>> show `bimap` identity

loadModule :: File -> Env 𝔹 -> Aff (Env 𝔹)
loadModule file ρ = do
   src <- loadFile (Folder "fluid/lib") file
   pure (successful (parse src module_ >>= desugarModuleFwd >>= eval_module ρ))

parseProgram :: Folder -> File -> Aff (S.Expr 𝔹)
parseProgram folder file = loadFile folder file <#> (successful <<< flip parse program)

open :: File -> Aff (S.Expr 𝔹)
open = parseProgram (Folder "fluid/example")

defaultImports :: Aff (Env 𝔹)
defaultImports =
   loadModule (File "prelude") primitives >>= loadModule (File "graphics") >>= loadModule (File "convolution")

openWithDefaultImports :: File -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithDefaultImports file = do
   ρ <- defaultImports
   open file <#> (ρ × _)

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: File -> Var -> Aff (Env 𝔹 × Env 𝔹)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   ρ <- defaultImports
   let _ × v = successful (desugarFwd s >>= eval ρ)
   pure (ρ × (Lin :- x ↦ v))
