module Module.Web where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, class MonadTrans, lift)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import DefiniteAssignment (class HasCxt)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import File (class LoadFile, FileCxt(..), loadFileFromPath)
import Val (class HasModuleStore, ModuleStore, emptyStore)

instance (MonadAff m, MonadError Error m, LoadFile m) => LoadFile (WebT m) where
   loadFileFromPath = lift <<< loadFileFromPath

newtype WebT m a = WebT (ReaderT FileCxt (StateT ModuleStore m) a)

runWebT :: forall m a. Monad m => FileCxt -> WebT m a -> m a
runWebT fileCxt (WebT x) = evalStateT (runReaderT x fileCxt) emptyStore

-- ======================
-- boilerplate
-- ======================

derive newtype instance Functor m => Functor (WebT m)
derive newtype instance Monad m => Apply (WebT m)
derive newtype instance Monad m => Applicative (WebT m)
derive newtype instance Monad m => Bind (WebT m)
derive newtype instance Monad m => Monad (WebT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (WebT m)
derive newtype instance MonadError Error m => MonadError Error (WebT m)
derive newtype instance MonadEffect m => MonadEffect (WebT m)
derive newtype instance MonadAff m => MonadAff (WebT m)
derive newtype instance Monad m => MonadAsk FileCxt (WebT m)
derive newtype instance Monad m => MonadReader FileCxt (WebT m)

instance Monad m => HasCxt (WebT m) where
   askCxt = WebT (ask <#> \(FileCxt { classCtx }) -> classCtx)

instance MonadTrans WebT where
   lift m = WebT (lift (lift m))

instance Monad m => HasModuleStore (WebT m) where
   getStore = WebT (lift get)
   modifyStore f = WebT (lift (modify_ f))
