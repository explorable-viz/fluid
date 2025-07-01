module Module.Web where

import Prelude

import Affjax (Error(..)) as A
import Affjax (Response)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (defaultRequest, printError, request)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, class MonadTrans, ExceptT(..), runExceptT)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import File (class LoadFile, File(..), Folder, prependFolder)
import Util (type (×), (×), AffError, debug, findM)

loadFile :: forall m. MonadError Error m => MonadAff m => Array Folder -> File -> m String
loadFile folders (File file) = do
   let urls = flip prependFolder (File $ file <> ".fld") <$> folders
   result <- runExceptT $ do
      _ × url' <- ExceptT $ liftAff $ findM urls checkUrl (Left A.RequestFailedError)
      when debug.logging $ liftAff $ log ("loadFile: resolved URL: " <> url')
      contents <- ExceptT $ liftAff $ request (defaultRequest { url = url', method = Left GET, responseFormat = string })
      pure contents.body
   either (throwError <<< E.error <<< printError) pure result
   where
   checkUrl :: File -> Aff (Either A.Error (Response String × String))
   checkUrl (File url) = do
      resp <- request (defaultRequest { url = url, method = Left HEAD, responseFormat = string })
      pure case resp of
         Right resp' | resp'.status == StatusCode 200 -> Right (resp' × url)
         Right _ -> Left A.RequestFailedError
         Left err -> Left err

loadFile' :: forall m. Array Folder -> File -> AffError m (File × String)
loadFile' folders file = (file × _) <$> loadFile folders file

newtype WebT (m :: Type -> Type) a = WebT (m a)

runWebT :: forall m a. WebT m a -> m a
runWebT (WebT x) = x

instance LoadFile (WebT m) where
   loadFile = loadFile

-- ======================
-- boilerplate
-- ======================

instance MonadTrans WebT where
   lift = WebT

derive newtype instance Functor m => Functor (WebT m)
derive newtype instance Apply m => Apply (WebT m)
derive newtype instance Applicative m => Applicative (WebT m)
derive newtype instance Bind m => Bind (WebT m)
derive newtype instance Monad m => Monad (WebT m)
derive newtype instance MonadThrow Error m => MonadThrow Error (WebT m)
derive newtype instance MonadError Error m => MonadError Error (WebT m)
derive newtype instance MonadEffect m => MonadEffect (WebT m)
derive newtype instance MonadAff m => MonadAff (WebT m)
