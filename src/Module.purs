module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff (Aff)
import EvalGraph (GraphConfig, eval, eval_module)
import Expr (traverseModule)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (WithGraphT, runWithGraphT, alloc, fresh)
import Lattice (botOf)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
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

parseProgram :: Folder -> File -> Aff (S.Expr Unit)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

loadModule :: forall s. Set s Vertex => File -> Env Vertex -> WithGraphT s Aff (Env Vertex)
loadModule file γα = do
   src <- lift $ lift $ loadFile (Folder "fluid/lib") file
   modα <- traverseModule (const fresh) (successful $ parse src (module_) >>= desugarModuleFwd)
   eval_module γα modα empty <#> (γα <+> _)

defaultImports :: forall s. Set s Vertex => WithGraphT s Aff (Env Vertex)
defaultImports = do
   γα <- traverse alloc primitives
   loadModule (File "prelude") γα >>= loadModule (File "graphics") >>= loadModule (File "convolution")

openWithDefaultImports :: forall g s. Graph g s => File -> Aff (GraphConfig g × S.Expr Unit)
openWithDefaultImports file = do
   (g × n) × γ <- successful <$> (runWithGraphT (G.empty × 0) $ defaultImports)
   s <- open file
   pure $ { g, n, γ } × s

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: forall g s. Graph g s => File -> Var -> Aff (GraphConfig g × Env Vertex)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   (g × n) × (γ × xv) <- successful <$>
      ( runWithGraphT (G.empty × 0) $ do
           γα <- defaultImports
           eα <- alloc (successful $ desug s)
           vα <- eval γα eα empty
           pure (γα × D.singleton x vα)
      )
   pure ({ g, n, γ } × xv)