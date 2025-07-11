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
import File (class LoadFile, File(..), FileCxt, FileCxt2(..), Folder, loadFile)
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

parseProgram :: forall m. LoadFile m => Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram folders file =
   loadFile folders file >>= flip parse P.program

module_ :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ folders file (ProgCxt r@{ mods }) = do
   when debug.logging $ log ("module_: " <> show (folders × file))
   src <- loadFile folders file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs folders (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram folders file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt2 m => LoadFile m => FileCxt -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt _ mods datasets = do
   FileCxt2 { fluidSrcPaths } <- ask
   pure (ProgCxt { fluidSrcPaths, primitives, mods: Nil, datasets: Nil })
      >>= concatM (File >>> module_ fluidSrcPaths <$> [ "lib/prelude" ] <> mods)
      >>= concatM (second File >>> datasetAs fluidSrcPaths <$> datasets)

initialConfig :: forall m a. MonadAff m => MonadError Error m => LoadFile m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prepConfig :: forall m. MonadAff m => MonadError Error m => LoadFile m => FileCxt -> File -> Raw ProgCxt -> m Config
prepConfig { fluidSrcPaths } file progCxt = do
   s <- parseProgram fluidSrcPaths file
   e <- desug s
   gconfig <- initialConfig e progCxt
   pure { s, e, gconfig }
