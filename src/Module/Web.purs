module Module.Web where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, class MonadTrans, lift)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import File (class LoadFile, File(..), FileCxt, Folder, fluidExtension, loadFile, loadFileFromPath_)
import Util (type (×), (×), AffError)

instance MonadThrow Error m => LoadFile (WebT m) where
   loadFileFromPath = loadFileFromPath_

loadFile_ :: forall m. LoadFile m => Array Folder -> File -> AffError m (File × String)
loadFile_ folders (File file) = (file_ × _) <$> loadFile folders file_
   where
   file_ = File (file <> fluidExtension)

newtype WebT :: forall k. (k -> Type) -> k -> Type
newtype WebT m a = WebT (ReaderT FileCxt m a)

runWebT :: forall m a. FileCxt -> WebT m a -> m a
runWebT fileCxt (WebT x) = runReaderT x fileCxt

-- ======================
-- boilerplate
-- ======================

derive newtype instance Functor m => Functor (WebT m)
derive newtype instance Apply m => Apply (WebT m)
derive newtype instance Applicative m => Applicative (WebT m)
derive newtype instance Bind m => Bind (WebT m)
derive newtype instance Monad m => Monad (WebT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (WebT m)
derive newtype instance MonadError Error m => MonadError Error (WebT m)
derive newtype instance MonadEffect m => MonadEffect (WebT m)
derive newtype instance MonadAff m => MonadAff (WebT m)
derive newtype instance MonadAsk FileCxt m => MonadAsk FileCxt (WebT m)
derive newtype instance Monad m => MonadReader FileCxt (WebT m)

instance MonadTrans WebT where
   lift m = WebT (lift m)
