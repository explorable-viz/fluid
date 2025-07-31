module Module.Node where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, try)
import Control.Monad.Except (class MonadError, class MonadTrans, lift)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import File (class LoadFile, File(..), FileCxt, prependFolder, loadFileFromPaths)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)
import Parse.Constants (str)
import Util (error, findM)

instance Monad m => LoadFile (NodeT m) where
   loadFile folders (File file) = loadFileFromPaths paths
      where
      paths = flip prependFolder (File $ file <> str.fluidExtension) <$> folders

   loadFileFromPaths paths = do
      path <- findM paths exists Nothing
      case path of
         Nothing -> error $ "Files " <> show paths <> " not found."
         Just name -> liftAff $ readTextFile UTF8 name
      where
      exists (File path) = do
         stats <- liftAff $ try (stat path)
         pure $ if either (const false) isFile stats then Just path else Nothing

newtype NodeT :: forall k. (k -> Type) -> k -> Type
newtype NodeT m a = NodeT (ReaderT FileCxt m a)

runNodeT :: forall m a. FileCxt -> NodeT m a -> m a
runNodeT fileCxt (NodeT x) = runReaderT x fileCxt

-- ======================
-- boilerplate
-- ======================

instance MonadTrans NodeT where
   lift m = NodeT (lift m)

derive newtype instance Functor m => Functor (NodeT m)
derive newtype instance Apply m => Apply (NodeT m)
derive newtype instance Applicative m => Applicative (NodeT m)
derive newtype instance Bind m => Bind (NodeT m)
derive newtype instance Monad m => Monad (NodeT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (NodeT m)
derive newtype instance MonadError Error m => MonadError Error (NodeT m)
derive newtype instance MonadEffect m => MonadEffect (NodeT m)
derive newtype instance MonadAff m => MonadAff (NodeT m)
derive newtype instance MonadAsk FileCxt m => MonadAsk FileCxt (NodeT m)
derive newtype instance Monad m => MonadReader FileCxt (NodeT m)
