module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Ann (Raw)
import Bindings (Var, (↦))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, ProgCxtEval(..), eval_progCxt)
import Expr (ProgCxt(..))
import Graph (empty) as G
import Graph.GraphImpl (GraphImpl)
import Graph.GraphWriter (alloc, runWithGraphAllocT)
import Parse (module_, program) as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util ((×), mapLeft)
import Util.Parse (SParser)

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

parseProgram :: forall m. MonadAff m => MonadError Error m => Folder -> File -> m (Raw S.Expr)
parseProgram folder file =
   loadFile folder file >>= flip parse P.program

open :: forall m. MonadAff m => MonadError Error m => File -> m (Raw S.Expr)
open = parseProgram (Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ file (ProgCxt r@{ mods }) = do
   src <- loadFile (Folder "fluid/lib") file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

defaultImports :: forall m. MonadAff m => MonadError Error m => m (Raw ProgCxt)
defaultImports =
   pure (ProgCxt { mods: Nil, datasets: Nil })
      >>= module_ (File "prelude")
      >>= module_ (File "graphics")
      >>= module_ (File "convolution")

datasetAs :: forall m. MonadAff m => MonadError Error m => File -> Var -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs file x (ProgCxt r@{ datasets }) = do
   eα <- parseProgram (Folder "fluid") file >>= desug
   pure $ ProgCxt r { datasets = x ↦ eα : datasets }

blah :: forall m. MonadError Error m => Raw ProgCxt -> m (GraphConfig GraphImpl)
blah progCxt = do
   (g × n) × progCxt' <- runWithGraphAllocT (G.empty × 0) do
      progCxt' <- alloc progCxt
      primitives' <- traverse alloc primitives
      γ <- eval_progCxt primitives' progCxt'
      pure $ ProgCxtEval { progCxt: progCxt', γ }
   pure { g, n, progCxt: progCxt' }
