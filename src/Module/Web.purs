module Module.Web where

import Prelude

import Affjax (Error(..)) as A
import Affjax (Response)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (defaultRequest, printError, request)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, class MonadTrans, ExceptT(..), lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import File (class LoadFile, File(..), FileCxt, Folder, loadFile, prependFolder, loadFileFromPaths)
import Parse.Constants (str)
import Util (type (×), (×), AffError, debug, findM)

instance MonadThrow Error m => LoadFile (WebT m) where
   loadFile folders (File file) = loadFileFromPaths paths
      where
      paths = flip prependFolder (File $ file <> str.fluidExtension) <$> folders

   loadFileFromPaths paths = do
      result <- runExceptT $ do
         _ × path' <- ExceptT $ liftAff $ findM paths checkPath (Left A.RequestFailedError)
         when debug.logging $ liftAff $ log ("loadFileFromPaths: resolved path: " <> path')
         contents <- ExceptT $ liftAff $ request (defaultRequest { url = path', method = Left GET, responseFormat = string })
         pure contents.body
      either (throwError <<< E.error <<< printError) pure result
      where
      checkPath :: File -> Aff (Either A.Error (Response String × String))
      checkPath (File path) = do
         resp <- request (defaultRequest { url = path, method = Left HEAD, responseFormat = string })
         pure case resp of
            Right resp' | resp'.status == StatusCode 200 -> Right (resp' × path)
            Right _ -> Left A.RequestFailedError
            Left err -> Left err

loadFile' :: forall m. LoadFile m => Array Folder -> File -> AffError m (File × String)
loadFile' folders file = (file × _) <$> loadFile folders file

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
