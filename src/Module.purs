module Module where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Data.Bifunctor (lmap)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (second)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, Expr, fv)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import Parse as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (desugarModuleFwd)
import SExpr as S
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, debug, (×))
import Util.Map (restrict)
import Util.Parse (SParser)

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. FileLoader m -> Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram loadFile folders file =
   loadFile folders file >>= flip parse P.program

module_ :: forall m. MonadAff m => MonadError Error m => FileLoader m -> Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ loadFile folders file (ProgCxt r@{ mods }) = do
   when debug.logging $ log ("module_: " <> show (folders × file))
   src <- loadFile folders file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => FileLoader m -> Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs loadFile folders (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram loadFile folders file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => FileContext m -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt { loadFile, fluidSrcPaths } mods datasets =
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (File >>> module_ loadFile fluidSrcPaths <$> [ "lib/prelude" ] <> mods)
      >>= concatM (second File >>> datasetAs loadFile fluidSrcPaths <$> datasets)

initialConfig :: forall m a. MonadError Error m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      -- Restrict γ derived from prog cxt to free vars for managability, although this precludes mapping back
      -- to surface syntax for now, and no easy way to similarly restrict inputs of corresponding graph.
      pure (progCxt' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prepConfig :: forall m. MonadAff m => MonadError Error m => FileContext m -> File -> Raw ProgCxt -> m Config
prepConfig { loadFile, fluidSrcPaths } file progCxt = do
   s <- parseProgram loadFile fluidSrcPaths file
   e <- desug s
   gconfig <- initialConfig e progCxt
   pure { s, e, gconfig }

type FileLoader m = Array Folder -> File -> AffError m String

type FileContext m =
   { loadFile :: FileLoader m
   , fluidSrcPaths :: Array Folder
   }

newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File
derive instance Newtype Folder _
derive newtype instance Show Folder

instance Semigroup Folder where
   append (Folder folder1) (Folder folder2) = Folder (folder1 <> "/" <> folder2)

prependFolder :: Folder -> File -> File
prependFolder (Folder folder) (File file) = File (folder <> "/" <> file)

infixr 5 prependFolder as </>
