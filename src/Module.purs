module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Ann (Raw)
import Bindings (Var)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List(..), reverse, (:))
import Data.Newtype (class Newtype)
import Data.Set (empty)
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval, eval_module)
import Expr (Module)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (class MonadWithGraphAlloc, alloc, fresh, runWithGraphAllocT)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util (type (×), (×), concatM, mapLeft)
import Util.Parse (SParser)
import Val (Env, ProgCxt(..), ProgCxtEval(..), (<+>))

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
      Left err -> throwError $ E.error $ printError err
      Right response -> pure response.body

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< mapLeft (E.error <<< show) <<< runParser src

parseProgram :: forall m. MonadAff m => MonadError Error m => Folder -> File -> m (S.Expr Unit)
parseProgram folder file =
   loadFile folder file >>= flip parse program

open :: forall m. MonadAff m => MonadError Error m => File -> m (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

loadModule :: forall m. MonadAff m => MonadWithGraphAlloc m => File -> ProgCxtEval Vertex -> m (ProgCxtEval Vertex)
loadModule file (ProgCxtEval r@{ progCxt: ProgCxt r'@{ mods }, γ }) = do
   src <- loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd >>= traverse (const fresh)
   γ' <- eval_module γ mod empty
   pure $ ProgCxtEval r { progCxt = ProgCxt r' { mods = mod : mods }, γ = γ <+> γ' }

defaultImports :: forall m. MonadAff m => MonadWithGraphAlloc m => m (ProgCxtEval Vertex)
defaultImports = do
   γ <- traverse alloc primitives
   loadModule (File "prelude") (ProgCxtEval { progCxt: ProgCxt { mods: Nil, datasets: Nil }, γ })
      >>= loadModule (File "graphics")
      >>= loadModule (File "convolution")

loadModule2 :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
loadModule2 file (ProgCxt r@{ mods }) = do
   src <- loadFile (Folder "fluid/lib") file
   mod <- parse src module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

defaultImports2 :: forall m. MonadAff m => MonadError Error m => m (Raw ProgCxt)
defaultImports2 =
   loadModule2 (File "prelude") (ProgCxt { mods: Nil, datasets: Nil })
      >>= loadModule2 (File "graphics")
      >>= loadModule2 (File "convolution")

loadDataset :: forall m. MonadAff m => MonadWithGraphAlloc m => File -> Raw ProgCxt -> m (ProgCxt Unit)
loadDataset file (ProgCxt r@{ datasets }) = do
   dataset <- parseProgram (Folder "fluid") file >>= desug
   pure $ ProgCxt r { datasets = dataset : datasets }

openDefaultImports :: forall m g. MonadAff m => MonadError Error m => Graph g => m (GraphConfig g)
openDefaultImports = do
   (g × n) × progCxt <- runWithGraphAllocT (G.empty × 0) defaultImports
   pure { g, n, progCxt }

openDatasetAs :: forall m g. MonadAff m => MonadError Error m => Graph g => File -> Var -> GraphConfig g -> m (GraphConfig g × Env Vertex)
openDatasetAs file x { g, n, progCxt: ProgCxtEval r@{ progCxt: ProgCxt r'@{ datasets }, γ } } = do
   s <- parseProgram (Folder "fluid") file
   (g' × n') × xv × progCxt <-
      runWithGraphAllocT (g × n) do
         eα <- desug s >>= alloc
         v <- eval γ eα empty
         pure $ D.singleton x v × ProgCxtEval (r { progCxt = ProgCxt (r' { datasets = eα : datasets }) })
   pure ({ g: g', n: n', progCxt } × xv)

eval_progCxt :: forall m. MonadWithGraphAlloc m => ProgCxt Vertex -> m (Env Vertex)
eval_progCxt (ProgCxt { mods }) = do
   traverse alloc primitives
      >>= concatM (reverse mods <#> addDefs)
   where
   addDefs :: Module Vertex -> Env Vertex -> m (Env Vertex)
   addDefs mod γ = do
      γ' <- eval_module γ mod empty
      pure $ γ <+> γ'

blah :: forall m g. Graph g => MonadError Error m => ProgCxt Unit -> m (GraphConfig g)
blah progCxt = do
   (g × n) × progCxt' <- runWithGraphAllocT (G.empty × 0) do
      progCxt' <- alloc progCxt
      γ <- eval_progCxt progCxt'
      pure $ ProgCxtEval { progCxt: progCxt', γ }
   pure { g, n, progCxt: progCxt' }
