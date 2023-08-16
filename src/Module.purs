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
import EvalGraph (eval, eval_module) as G
import Expr (traverseModule)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (WithGraphT, runWithGraphT, alloc, alloc')
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

type GraphConfig g =
   { g :: g
   , n :: Int
   , γα :: Env Vertex
   , s :: S.Expr Unit
   }

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
   modα <- traverseModule alloc' (successful $ parse src (module_) >>= desugarModuleFwd)
   G.eval_module γα modα empty <#> (γα <+> _)

defaultImports :: forall s. Set s Vertex => WithGraphT s Aff (Env Vertex)
defaultImports = do
   γα <- traverse alloc primitives
   loadModule (File "prelude") γα >>= loadModule (File "graphics") >>= loadModule (File "convolution")

openWithDefaultImports :: forall g s. Graph g s => File -> Aff (GraphConfig g)
openWithDefaultImports file = do
   (g × n) × γα × s <- successful <$> (runWithGraphT (G.empty × 0) $ defaultImports >>= \γ -> lift $ lift $ open file <#> (γ × _))
   pure { g, n, γα, s }

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: forall g s. Graph g s => File -> Var -> Aff (GraphConfig g × Env Vertex)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   (g × n) × (γα × xvα) <- successful <$>
      ( runWithGraphT (G.empty × 0) $ do
           γα <- defaultImports
           eα <- alloc (successful $ desug s)
           vα <- G.eval γα eα empty
           pure (γα × D.singleton x vα)
      )
   pure ({ g, n, γα, s } × xvα)