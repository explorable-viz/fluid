module Module.Node where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, try)
import Control.Monad.Except (class MonadError, class MonadTrans)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import File (class LoadFile, File(..), FileLoader, prependFolder)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)
import Node.FS.Stats (isFile)
import Util (error, findM)

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

newtype NodeT (m :: Type -> Type) a = NodeT (m a)

runNodeT :: forall m a. NodeT m a -> m a
runNodeT (NodeT x) = x

instance MonadAff (NodeT m) => LoadFile (NodeT m) where
   loadFile' folders file = loadFile folders file

-- ======================
-- boilerplate
-- ======================

instance MonadTrans NodeT where
   lift = NodeT

derive newtype instance Functor m => Functor (NodeT m)
derive newtype instance Apply m => Apply (NodeT m)
derive newtype instance Applicative m => Applicative (NodeT m)
derive newtype instance Bind m => Bind (NodeT m)
derive newtype instance Monad m => Monad (NodeT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (NodeT m)
derive newtype instance MonadError Error m => MonadError Error (NodeT m)
derive newtype instance MonadEffect m => MonadEffect (NodeT m)
derive newtype instance MonadAff m => MonadAff (NodeT m)
