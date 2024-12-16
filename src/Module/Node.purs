module Module.Node
   ( loadFile
   , parseProgram
   , open
   , module_
   , datasetAs
   , loadProgCxt
   , module F
   , module Module
   , prepConfig
   ) where

import Prelude

import Bind (Bind)
import Control.Monad.Except (class MonadError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Lattice (Raw)
import Module (Config, initialConfig, parse)
import Module (File(..), Folder(..), FileLoader) as F
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (AffError)

loadFile :: F.FileLoader
loadFile (F.Folder folder) (F.File file) = do
   let url = folder <> "/" <> file <> ".fld"
   buffer <- liftAff $ readTextFile ASCII url
   pure buffer

parseProgram ∷ ∀ m. F.Folder -> F.File → AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

open :: forall m. F.File -> AffError m (Raw S.Expr)
open = parseProgram (F.Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => F.File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Bind F.File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt = M.loadProgCxt loadFile

prepConfig :: forall m. MonadAff m => MonadError Error m => F.File -> ProgCxt Unit -> m Config
prepConfig = M.prepConfig loadFile
