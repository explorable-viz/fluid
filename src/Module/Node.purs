module Module.Node
   ( loadFile
   , parseProgram
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

loadFile :: forall m. F.FileLoader m
loadFile (F.Folder folder) (F.File file) = do
   let url = folder <> "/" <> file <> ".fld"
   buffer <- liftAff $ readTextFile UTF8 url
   pure buffer

parseProgram ∷ ∀ m. F.Folder -> F.File → AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => F.Folder -> F.File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => F.Folder -> Bind F.File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPaths = M.loadProgCxt { loadFile, fluidSrcPaths }

prepConfig :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> F.File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPaths = M.prepConfig { loadFile, fluidSrcPaths }
