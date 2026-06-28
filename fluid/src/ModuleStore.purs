module ModuleStore where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Map (Map)
import Graph (Vertex)
import ModuleGraph (ModuleName)
import Val (Env)

type ModuleStore = Map ModuleName (Env Vertex)

class Monad m <= HasModuleStore m where
   getStore :: m ModuleStore
   modifyStore :: (ModuleStore -> ModuleStore) -> m Unit

instance (Monad m, HasModuleStore m) => HasModuleStore (StateT s m) where
   getStore = lift getStore
   modifyStore = lift <<< modifyStore

instance (Monad m, HasModuleStore m) => HasModuleStore (ReaderT r m) where
   getStore = lift getStore
   modifyStore = lift <<< modifyStore

instance (Monad m, HasModuleStore m) => HasModuleStore (ExceptT e m) where
   getStore = lift getStore
   modifyStore = lift <<< modifyStore

instance (Monad m, HasModuleStore m, Monoid w) => HasModuleStore (WriterT w m) where
   getStore = lift getStore
   modifyStore = lift <<< modifyStore
