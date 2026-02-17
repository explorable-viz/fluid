module Module.Node where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, try)
import Control.Monad.Except (class MonadError, class MonadTrans, lift)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import File (class LoadFile, File(..), FileCxt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)

instance Monad m => LoadFile (NodeT m) where
   loadFileFromPath (File path) = do
      stats <- liftAff $ try (stat path)
      case stats of
         Right s | isFile s -> Just <$> liftAff (readTextFile UTF8 path)
         _ -> pure Nothing

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
