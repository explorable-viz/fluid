module Module where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Bifunctor (lmap)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (second)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, Expr, fv)
import File (class LoadFile, File(..), FileCxt(..), Folder, loadFile)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import Parse (asModule, standalone)
import Parse as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Module, desugarModuleFwd)
import SExpr as S
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, debug, (×))
import Util.Map (restrict)
import Util.Parse (SParser)

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. LoadFile m => Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram folders file =
   loadFile folders file >>= flip parse (standalone P.program)

parseProgramAsModule :: forall m. LoadFile m => Array Folder -> File -> AffError m (Module (Raw S.Expr))
parseProgramAsModule folders file =
   loadFile folders file >>= flip parse (asModule file P.program)

module_ :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ folders file (ProgCxt r@{ mods }) = do
   when debug.logging $ log ("module_: " <> show (folders × file))
   src <- loadFile folders file
   { content } <- parse src (asModule file P.module_)
   mod' <- desugarModuleFwd content
   pure $ ProgCxt r { mods = mod' : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs folders (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram folders file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt datasets = do
   FileCxt { fluidSrcPaths } <- ask
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (second File >>> datasetAs fluidSrcPaths <$> datasets)

-- updates a progCxt with imported modules
loadMods :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array String -> Raw ProgCxt -> m (Raw ProgCxt)
loadMods mods progCxt = do
   FileCxt { fluidSrcPaths } <- ask
   concatM (File >>> module_ fluidSrcPaths <$> [ "lib/prelude" ] <> mods) progCxt

initialConfig :: forall m a. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prepConfig :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => File -> Raw ProgCxt -> m Config
prepConfig file progCxt = do
   FileCxt { fluidSrcPaths } <- ask
   { content: s, imports } <- parseProgramAsModule fluidSrcPaths file
   e <- desug s
   progCxt' <- loadMods imports progCxt
   gconfig <- initialConfig e progCxt'
   pure { s, e, gconfig }
