module Module.Local where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Data.Bifunctor (lmap)
import Data.List (List(..), (:))
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
import Module (Folder(..), File(..), Loader)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parse (module_, program) as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, (×))
import Util.Map (restrict)
import Util.Parse (SParser)

loadFile :: Loader
loadFile (Folder folder) (File file) = do
   let url = folder <> "/" <> file <> ".fld"
   buffer <- liftAff $ readTextFile ASCII url
   pure buffer

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. Loader -> Folder -> File -> AffError m (Raw S.Expr)
parseProgram load folder file =
   load folder file >>= flip parse P.program

open :: forall m. Folder -> File -> AffError m (Raw S.Expr)
open = parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ root file (ProgCxt r@{ mods }) = do
   src <- loadFile root file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs (Folder root) (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram loadFile (Folder $ root <> "/dataset") file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt root mods datasets =
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (File >>> module_ root <$> [ "lib/prelude" ] <> mods)
      >>= concatM (second File >>> datasetAs root <$> datasets)

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
