module Module.Node where

import Prelude

import Bind (Bind)
import Control.Monad.Except (class MonadError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import EvalGraph (GraphConfig)
import Expr (class FV)
import Lattice (Raw)
import Module.Files (File(..), Folder(..), FileLoader)
import Module (datasetAs, loadProgCxt, module_, parse, parseProgram, initialConfig) as M
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (AffError)
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
datasetAs = M.datasetAs loadFile (Folder "../fluid/fluid")

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt = M.loadProgCxt loadFile (Folder "../fluid/fluid")

initialConfig :: forall m a. MonadError Error m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig = M.initialConfig