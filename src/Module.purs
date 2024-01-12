module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (second)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, fv)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import Parse (module_, program) as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Test.Util.Debug (checking)
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

parseProgram :: forall m. Folder -> File -> AffError m (Raw S.Expr)
parseProgram folder file =
   loadFile folder file >>= flip parse P.program

open :: forall m. File -> AffError m (Raw S.Expr)
open = parseProgram (Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ file (ProgCxt r@{ mods }) = do
   src <- loadFile (Folder "fluid") file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram (Folder "fluid") file >>= desug
   pure $ ProgCxt r { datasets = x ↦ eα : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt mods datasets =
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= ((File <$> [ "lib/prelude" ] <> mods) <#> module_ # concatM)
      >>= ((second File <$> datasets) <#> datasetAs # concatM)

initialConfig :: forall m a. MonadError Error m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      -- Restrict γ derived from prog cxt to free vars for managability, although this precludes mapping back
      -- to surface syntax for now, and no easy way to similarly restrict inputs of corresponding graph.
      pure (progCxt' × γ `restrict` (fv e))
   pure { n, progCxt: progCxt', γ }
