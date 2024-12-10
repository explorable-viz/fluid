module Module.Node where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Except (class MonadError)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (second)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, fv)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import Module (Folder(..), File(..), FileLoader)
import Module (parseProgram, parse, module_) as M
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Expr) as S
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, (×))
import Util.Map (restrict)
import Util.Parse (SParser)

loadFile :: FileLoader
loadFile (Folder folder) (File file) = do
   let url = folder <> "/" <> file <> ".fld"
   buffer <- liftAff $ readTextFile ASCII url
   pure buffer

parseProgram ∷ ∀ m. Folder → File → AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

parse ∷ ∀ a m. MonadError Error m ⇒ String → SParser a → m a
parse = M.parse

open :: forall m. File -> AffError m (Raw S.Expr)
open = parseProgram (Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile (Folder "../fluid/fluid")

datasetAs :: forall m. MonadAff m => MonadError Error m => Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram (Folder "../fluid/fluid/dataset") file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt mods datasets =
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (File >>> module_ <$> [ "lib/prelude" ] <> mods)
      >>= concatM (second File >>> datasetAs <$> datasets)

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
