module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype)
import Data.Set (empty)
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import EvalGraph (GraphConfig, eval, eval_module)
import Expr (traverseModule)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (class MonadGraphAlloc, alloc, fresh, runWithGraphAllocT)
import Lattice (botOf)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util (type (×), (×), error, successful, fromRight)
import Util.Parse (SParser)
import Val (Env, (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

loadFile :: Folder -> File -> Aff String
loadFile (Folder folder) (File file) = do
   let url = "./" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

parse :: forall a m. MonadError String m => String -> SParser a -> m a
parse src p = case runParser src p of
   Left err -> throwError $ show err
   Right x -> pure x

parseProgram :: Folder -> File -> Aff (S.Expr Unit)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

loadModule :: forall m. MonadAff m => MonadGraphAlloc m => File -> Env Vertex -> m (Env Vertex)
loadModule file γ = do
   src <- liftAff $ loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd >>= traverseModule (const fresh)
   eval_module γ mod empty <#> (γ <+> _)

defaultImports :: forall m. MonadAff m => MonadGraphAlloc m => m (Env Vertex)
defaultImports = do
   γα <- traverse alloc primitives
   loadModule (File "prelude") γα >>= loadModule (File "graphics") >>= loadModule (File "convolution")

-- | Evaluates default imports from empty initial graph config
openDefaultImports :: forall g. Graph g => Aff (GraphConfig g)
openDefaultImports = do
   (g × n) × γα <- fromRight <$> runWithGraphAllocT (G.empty × 0) defaultImports
   pure $ { g, n, γα }

-- | Evaluate dataset in context of existing graph config
openDatasetAs :: forall g. Graph g => File -> Var -> GraphConfig g -> Aff (GraphConfig g × Env Vertex)
openDatasetAs file x { g, n, γα } = do
   s <- parseProgram (Folder "fluid") file
   (g' × n') × (γα' × xv) <- fromRight <$>
      ( runWithGraphAllocT (g × n) $ do
           e <- desug s
           eα <- alloc e
           vα <- eval γα eα empty
           pure (γα × D.singleton x vα)
      )
   pure ({ g: g', n: n', γα: γα' } × xv)
