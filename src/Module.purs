module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var, (↦))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, fv)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT)
import Lattice (Raw)
import Parse (module_, program) as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Util (type (×), AffError, concatM, mapLeft, (×))
import Util.Parse (SParser)
import Val (restrict)

newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

loadFile :: forall m. Folder -> File -> AffError m String
loadFile (Folder folder) (File file) = do
   let url = "./" <> folder <> "/" <> file <> ".fld"
   result <- liftAff $ request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> throwError $ E.error $ printError err
      Right response -> pure response.body

loadFile' :: forall m. Folder -> File -> AffError m (File × String)
loadFile' folder file = (file × _) <$> loadFile folder file

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< mapLeft (E.error <<< show) <<< runParser src

parseProgram :: forall m. MonadAff m => MonadError Error m => Folder -> File -> m (Raw S.Expr)
parseProgram folder file =
   loadFile folder file >>= flip parse P.program

open :: forall m. MonadAff m => MonadError Error m => File -> m (Raw S.Expr)
open = parseProgram (Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ file (ProgCxt r@{ mods }) = do
   src <- loadFile (Folder "fluid") file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

modules :: forall m. MonadAff m => MonadError Error m => Array File -> Raw ProgCxt -> m (Raw ProgCxt)
modules files = files <#> module_ # concatM

prelude :: forall m. MonadAff m => MonadError Error m => m (Raw ProgCxt)
prelude =
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil }) >>= modules [ File "lib/prelude" ]

datasetAs :: forall m. MonadAff m => MonadError Error m => File -> Var -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs file x (ProgCxt r@{ datasets }) = do
   eα <- parseProgram (Folder "fluid") file >>= desug
   pure $ ProgCxt r { datasets = x ↦ eα : datasets }

initialConfig :: forall m a. MonadError Error m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   n × _ × progCxt' × γ <- runAllocT 0 do
      progCxt' <- alloc progCxt
      _ × γ <- runWithGraphT (eval_progCxt progCxt') :: AllocT m (GraphImpl × _)
      pure (progCxt' × γ `restrict` (fv e))
   -- restricting to free vars makes γ more managable, but precludes mapping back to surface syntax for now
   pure { n, progCxt: progCxt', γ }
