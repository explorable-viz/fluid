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
import File (class LoadFile, File(..), FileCxt2, prependFolder)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)
import Util (error, findM)

instance (MonadAff m, MonadError Error many) => LoadFile (NodeT m) where
   loadFile folders (File file) = do
      let urls = flip prependFolder (File $ file <> ".fld") <$> folders
      url <- findM urls exists Nothing
      case url of
         Nothing -> error $ "File " <> file <> " not found."
         Just name -> liftAff $ readTextFile UTF8 name
      where
      exists (File url) = do
         stats <- liftAff $ try (stat url)
         pure $ if either (const false) isFile stats then Just url else Nothing

instance (MonadAff m, MonadError Error m) => LoadFile (NodeT2 m) where
   loadFile folders (File file) = do
      let urls = flip prependFolder (File $ file <> ".fld") <$> folders
      url <- findM urls exists Nothing
      case url of
         Nothing -> error $ "File " <> file <> " not found."
         Just name -> liftAff $ readTextFile UTF8 name
      where
      exists (File url) = do
         stats <- liftAff $ try (stat url)
         pure $ if either (const false) isFile stats then Just url else Nothing

newtype NodeT (m :: Type -> Type) a = NodeT (m a)

newtype NodeT2 :: forall k. (k -> Type) -> k -> Type
newtype NodeT2 m a = NodeT2 (ReaderT FileCxt2 m a)

runNodeT :: forall m a. NodeT m a -> m a
runNodeT (NodeT x) = x

runNodeT2 :: forall m a. FileCxt2 -> NodeT2 m a -> m a
runNodeT2 fileCxt (NodeT2 x) = runReaderT x fileCxt

-- ======================
-- boilerplate
-- ======================

instance MonadTrans NodeT where
   lift = NodeT

instance MonadTrans NodeT2 where
   lift m = NodeT2 (lift m)

derive newtype instance Functor m => Functor (NodeT m)
derive newtype instance Apply m => Apply (NodeT m)
derive newtype instance Applicative m => Applicative (NodeT m)
derive newtype instance Bind m => Bind (NodeT m)
derive newtype instance Monad m => Monad (NodeT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (NodeT m)
derive newtype instance MonadError Error m => MonadError Error (NodeT m)
derive newtype instance MonadEffect m => MonadEffect (NodeT m)
derive newtype instance MonadAff m => MonadAff (NodeT m)
derive newtype instance MonadAsk FileCxt2 m => MonadAsk FileCxt2 (NodeT m)
derive newtype instance MonadReader FileCxt2 m => MonadReader FileCxt2 (NodeT m)

derive newtype instance Functor m => Functor (NodeT2 m)
derive newtype instance Apply m => Apply (NodeT2 m)
derive newtype instance Applicative m => Applicative (NodeT2 m)
derive newtype instance Bind m => Bind (NodeT2 m)
derive newtype instance Monad m => Monad (NodeT2 m)
derive newtype instance MonadThrow Error m => MonadThrow Error (NodeT2 m)
derive newtype instance MonadError Error m => MonadError Error (NodeT2 m)
derive newtype instance MonadEffect m => MonadEffect (NodeT2 m)
derive newtype instance MonadAff m => MonadAff (NodeT2 m)
derive newtype instance MonadAsk FileCxt2 m => MonadAsk FileCxt2 (NodeT2 m)
derive newtype instance MonadReader FileCxt2 m => MonadReader FileCxt2 (NodeT2 m)
