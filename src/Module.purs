module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var)
import Control.Monad.Except (except)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype)
import Data.Set (empty) as S
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff (Aff)
import EvalGraph (GraphConfig, eval, eval_module)
import Expr (traverseModule)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (WithGraphAllocT, runWithGraphAllocT, alloc, fresh)
import Lattice (botOf)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util (MayFailT, type (×), (×), error, successful, fromRight)
import Util.Parse (SParser)
import Val (Env, (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File
derive instance Newtype File _

resourceServerUrl :: String
resourceServerUrl = "."

loadFile :: Folder -> File -> Aff String
loadFile (Folder folder) (File file) = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

parse :: forall t m. Monad m => String -> SParser t -> MayFailT m t
parse src = runParser src >>> show `bimap` identity >>> except

parseProgram :: Folder -> File -> Aff (S.Expr Unit)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

loadModule :: File -> Env Vertex -> WithGraphAllocT Aff (Env Vertex)
loadModule file γ = do
   src <- lift $ lift $ lift $ loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd >>= traverseModule (const fresh)
   eval_module γ mod S.empty <#> (γ <+> _)

defaultImports :: WithGraphAllocT Aff (Env Vertex)
defaultImports = do
   γα <- traverse alloc primitives
   loadModule (File "prelude") γα >>= loadModule (File "graphics") >>= loadModule (File "convolution")

-- | Evaluates the default imports from an empty initial graph config
openDefaultImports :: forall g. Graph g => Aff (GraphConfig g)
openDefaultImports = do
   (g × n) × γα <- fromRight <$> runWithGraphAllocT (G.empty × 0) defaultImports
   pure $ { g, n, γα }

-- | Evaluates a dataset from an existing graph config (produced by openDefaultImports)
openDatasetAs :: forall g. Graph g => File -> Var -> GraphConfig g -> Aff (GraphConfig g × Env Vertex)
openDatasetAs file x { g, n, γα } = do
   s <- parseProgram (Folder "fluid") file
   (g' × n') × (γα' × xv) <- fromRight <$>
      ( runWithGraphAllocT (g × n) $ do
           e <- desug s
           eα <- alloc e
           vα <- eval γα eα S.empty
           pure (γα × D.singleton x vα)
      )
   pure ({ g: g', n: n', γα: γα' } × xv)
