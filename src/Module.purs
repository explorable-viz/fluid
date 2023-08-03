module Module where

import Prelude
import Affjax.Web (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Parsing (runParser)
import Bindings (Var)
import Desugarable (desug)
import Dict (singleton) as D
import Eval (eval, eval_module)
import Lattice (𝔹, bot, botOf)
import Parse (module_, program)
import Primitive.Defs (primitives)
import SExpr (desugarModuleFwd)
import SExpr (Expr) as S
import Util (MayFail, type (×), (×), error, successful)
import Util.Parse (SParser)
import Val (Env, (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

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

parse :: forall t. String -> SParser t -> MayFail t
parse src = runParser src >>> show `bimap` identity

loadModule :: File -> Env 𝔹 -> Aff (Env 𝔹)
loadModule file γ = do
   src <- loadFile (Folder "fluid/lib") file
   pure $ successful $
      (parse src (module_ <#> botOf) >>= desugarModuleFwd >>= flip (eval_module γ) bot) <#> (γ <+> _)

parseProgram :: Folder -> File -> Aff (S.Expr 𝔹)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr 𝔹)
open = parseProgram (Folder "fluid/example")

defaultImports :: Aff (Env 𝔹)
defaultImports =
   loadModule (File "prelude") (primitives <#> botOf) >>= loadModule (File "graphics") >>= loadModule (File "convolution")

openWithDefaultImports :: File -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithDefaultImports file = do
   γ <- defaultImports
   open file <#> (γ × _)

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: File -> Var -> Aff (Env 𝔹 × Env 𝔹)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   γ <- defaultImports
   let _ × v = successful (desug s >>= flip (eval γ) bot)
   pure (γ × D.singleton x v)
