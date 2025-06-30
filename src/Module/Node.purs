module Module.Node where

import Prelude

import Bind (Bind)
import Control.Monad.Error.Class (class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (class MonadError, class MonadTrans, lift)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import File (class MonadAffLoadFile, File(..), FileLoader, Folder, prependFolder)
import Lattice (Raw)
import Module (Config)
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (AffError, error, findM)

loadFile :: forall m. FileLoader m
loadFile folders (File file) = do
   let urls = flip prependFolder (File $ file <> ".fld") <$> folders
   url <- findM urls exists Nothing
   case url of
      Nothing -> error $ "File " <> file <> " not found."
      Just name -> liftAff $ readTextFile UTF8 name
   where
   exists :: File -> m (Maybe String)
   exists (File url) = do
      stats <- liftAff $ try (stat url)
      pure $ if (either (const false) isFile stats) then Just url else Nothing

parseProgram ∷ ∀ m. Array Folder -> File → AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPaths = M.loadProgCxt { loadFile, fluidSrcPaths }

prepConfig :: forall m. MonadAff m => MonadError Error m => Array Folder -> File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPaths = M.prepConfig { loadFile, fluidSrcPaths }

newtype NodeT (m :: Type -> Type) a = NodeT (m a)

runNodeT :: forall m a. NodeT m a -> m a
runNodeT (NodeT x) = x

-- ======================
-- boilerplate
-- ======================

instance MonadTrans NodeT where
   lift = NodeT

derive instance Functor m => Functor (NodeT m)

instance Apply m => Apply (NodeT m) where
   apply (NodeT fs) (NodeT xs) = NodeT (fs <*> xs)

instance Applicative m => Applicative (NodeT m) where
   pure = NodeT <<< pure

instance Bind m => Bind (NodeT m) where
   bind (NodeT x) f = NodeT $ x >>= runNodeT <<< f

instance Monad m => Monad (NodeT m)

instance (Monad (NodeT m), MonadThrow Error m) => MonadThrow Error (NodeT m) where
   throwError = lift <<< throwError

instance (MonadError Error m, MonadThrow Error (NodeT m)) => MonadError Error (NodeT m) where
   catchError (NodeT x) h =
      NodeT $ catchError x \e -> runNodeT (h e)

instance MonadAff m => MonadAffLoadFile (NodeT m) where
   loadFile' folders file = loadFile folders file
