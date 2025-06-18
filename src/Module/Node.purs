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
import Control.Monad.Error.Class (try)
import Control.Monad.Except (class MonadError)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Lattice (Raw)
import Module (Config, initialConfig, parse, prependFolder)
import Module (File(..), Folder(..), FileLoader) as F
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (AffError, error, findM)

loadFile :: forall m. F.FileLoader m
loadFile folders (F.File file) = do
   let urls = flip prependFolder (F.File $ file <> ".fld") <$> folders
   url <- findM urls exists Nothing
   case url of
      Nothing -> error $ "File " <> file <> " not found."
      Just name -> liftAff $ readTextFile UTF8 name
   where
   exists :: F.File -> m (Maybe String)
   exists (F.File url) = do
      stats <- liftAff $ try (stat url)
      pure $ if (either (const false) isFile stats) then Just url else Nothing

parseProgram ∷ ∀ m. Array F.Folder -> F.File → AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> F.File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> Bind F.File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPaths = M.loadProgCxt { loadFile, fluidSrcPaths }

prepConfig :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> F.File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPaths = M.prepConfig { loadFile, fluidSrcPaths }
