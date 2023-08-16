module Module where

import Prelude
import Affjax.Web (defaultRequest, printError, request)
import Affjax.ResponseFormat (string)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Parsing (runParser)
import Bindings (Var)
import Desugarable (desug)
import Dict (singleton) as D
import Eval (eval, eval_module)
import EvalGraph (eval_module) as G
import Graph.GraphWriter (WithGraphT)
import Graph (class Graph, Vertex)
-- import Graph (empty) as G
import Lattice (𝔹, bot, botOf)
import Parse (module_, program)
import Primitive.Defs (primitives)
import SExpr (desugarModuleFwd)
import SExpr (Expr) as S
import Set (class Set, empty)
import Util (MayFail, type (×), (×), error, successful)
import Util.Parse (SParser)
import Val (Env, (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

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

parseProgram :: Folder -> File -> Aff (S.Expr 𝔹)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr 𝔹)
open = parseProgram (Folder "fluid/example")

loadModule :: File -> Env 𝔹 -> Aff (Env 𝔹)
loadModule file γ = do
   src <- loadFile (Folder "fluid/lib") file
   pure $ successful $
      (parse src (module_ <#> botOf) >>= desugarModuleFwd >>= flip (eval_module γ) bot) <#> (γ <+> _)

loadModuleG :: forall s. Set s Vertex => File -> Env Vertex -> WithGraphT s Aff (Env Vertex)
loadModuleG file γα = do
   src <- lift $ lift $ loadFile (Folder "fluid/lib") file
   let mod = successful $ parse src (module_) >>= desugarModuleFwd
   G.eval_module γα mod empty <#> (γα <+> _)

defaultImports :: Aff (Env 𝔹)
defaultImports =
   loadModule (File "prelude") (primitives <#> botOf) >>= loadModule (File "graphics") >>= loadModule (File "convolution")

-- defaultImportsG :: forall s. Set s Vertex => WithGraphT s Aff (Env Vertex)
-- defaultImportsG = do
--    γα <- evalEnv primitives
--    loadModule (File "prelude") γα >>= loadModule (File "graphics") >>= loadModule (File "convolution")

openWithDefaultImports :: File -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithDefaultImports file = do
   γ <- defaultImports
   open file <#> (γ × _)

-- openWithDefaultImports :: forall g s. Graph g s => File -> Aff ((g × Int) × Env 𝔹 × S.Expr 𝔹)
-- openWithDefaultImports file = successful <$>
--    ( runWithGraphT (G.empty × 0) $ do
--         γ <- defaultImports
--         lift $ lift $ open file <#> (γ × _)
--    )

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: File -> Var -> Aff (Env 𝔹 × Env 𝔹)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   γ <- defaultImports
   let _ × v = successful (desug s >>= flip (eval γ) bot)
   pure (γ × D.singleton x v)

-- -- Return ambient environment used to load dataset along with new binding.
-- openDatasetAs :: forall g s. Graph g s => File -> Var -> Aff ((g × Int) × Env 𝔹 × Env 𝔹)
-- openDatasetAs file x = successful <$>
--    ( runWithGraphT (G.empty × 0) $ do
--         s <- lift $ lift $ parseProgram (Folder "fluid") file
--         γ <- defaultImports
--         let _ × v = successful (desug s >>= flip (eval γ) bot)
--         pure (γ × D.singleton x v)
--    )