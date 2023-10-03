module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Set (empty)
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import EvalGraph (GraphConfig, eval, eval_module)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (class MonadWithGraphAlloc, alloc, fresh, runWithGraphAllocT)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util (type (×), concatM, mapLeft, (×))
import Util.Parse (SParser)
import Val (Env, ProgCxt(..), ProgCxt2(..), (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

loadFile :: forall m. MonadAff m => MonadError Error m => Folder -> File -> m String
loadFile (Folder folder) (File file) = do
   let url = "./" <> folder <> "/" <> file <> ".fld"
   result <- liftAff $ request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> throwError $ error $ printError err
      Right response -> pure response.body

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< mapLeft (error <<< show) <<< runParser src

parseProgram :: forall m. MonadAff m => MonadError Error m => Folder -> File -> m (S.Expr Unit)
parseProgram folder file =
   loadFile folder file >>= flip parse program

open :: forall m. MonadAff m => MonadError Error m => File -> m (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

loadModule :: forall m. MonadAff m => MonadWithGraphAlloc m => File -> ProgCxt Vertex -> m (ProgCxt Vertex)
loadModule file (ProgCxt r@{ mods, γ }) = do
   src <- loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd >>= traverse (const fresh)
   γ' <- eval_module γ mod empty
   pure $ ProgCxt r { mods = mod : mods, γ = γ <+> γ' }

defaultImports :: forall m. MonadAff m => MonadWithGraphAlloc m => m (ProgCxt Vertex)
defaultImports = do
   γ <- traverse alloc primitives
   loadModule (File "prelude") (ProgCxt { mods: Nil, datasets: Nil, γ })
      >>= loadModule (File "graphics")
      >>= loadModule (File "convolution")

loadModule2 :: forall m. MonadAff m => MonadWithGraphAlloc m => File -> ProgCxt2 Unit -> m (ProgCxt2 Unit)
loadModule2 file (ProgCxt2 r@{ mods }) = do
   src <- loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd
   pure $ ProgCxt2 r { mods = mod : mods }

defaultImports2 :: forall m. MonadAff m => MonadWithGraphAlloc m => m (ProgCxt2 Unit)
defaultImports2 =
   loadModule2 (File "prelude") (ProgCxt2 { mods: Nil, datasets: Nil })
      >>= loadModule2 (File "graphics")
      >>= loadModule2 (File "convolution")

loadDataset :: forall m. MonadAff m => MonadWithGraphAlloc m => File -> ProgCxt2 Unit -> m (ProgCxt2 Unit)
loadDataset file (ProgCxt2 r@{ datasets }) = do
   dataset <- parseProgram (Folder "fluid") file >>= desug
   pure $ ProgCxt2 r { datasets = dataset : datasets }

openDefaultImports :: forall m g. MonadAff m => MonadError Error m => Graph g => m (GraphConfig g)
openDefaultImports = do
   (g × n) × progCxt <- runWithGraphAllocT (G.empty × 0) defaultImports
   pure { g, n, progCxt }

openDatasetAs :: forall m g. MonadAff m => MonadError Error m => Graph g => File -> Var -> GraphConfig g -> m (GraphConfig g × Env Vertex)
openDatasetAs file x { g, n, progCxt: ProgCxt r@{ γ, datasets } } = do
   s <- parseProgram (Folder "fluid") file
   (g' × n') × xv × progCxt <-
      runWithGraphAllocT (g × n) do
         eα <- desug s >>= alloc
         v <- eval γ eα empty
         pure $ D.singleton x v × ProgCxt (r { datasets = eα : datasets })
   pure ({ g: g', n: n', progCxt } × xv)

eval_progCxt :: forall m. MonadWithGraphAlloc m => ProgCxt2 Vertex -> m (Env Vertex)
eval_progCxt (ProgCxt2 { mods }) =
   traverse alloc primitives
      >>= concatM (mods <#> \mod γ' -> eval_module γ' mod empty)
