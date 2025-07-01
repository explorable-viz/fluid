module Module.Web where

import Prelude

import Affjax (Error(..)) as A
import Affjax (Response)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (defaultRequest, printError, request)
import Bind (Bind)
import Control.Monad.Error.Class (class MonadThrow, catchError, throwError)
import Control.Monad.Except (class MonadError, class MonadTrans, ExceptT(..), lift, runExceptT)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import File (class LoadFile, File(..), FileLoader, Folder, prependFolder)
import Lattice (Raw)
import Module (datasetAs, module_, parseProgram) as M
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (type (×), (×), AffError, debug, findM)

loadFile :: forall m. FileLoader m
loadFile folders (File file) = do
   let urls = flip prependFolder (File $ file <> ".fld") <$> folders
   result <- runExceptT $ do
      (_ × (url')) <- ExceptT $ liftAff $ findM urls checkUrl (Left A.RequestFailedError)
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

parseProgram :: forall m. Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

newtype WebT (m :: Type -> Type) a = WebT (m a)

runWebT :: forall m a. WebT m a -> m a
runWebT (WebT x) = x

instance LoadFile (WebT m) where
   loadFile' folders file = loadFile folders file

-- ======================
-- boilerplate
-- ======================

instance MonadTrans WebT where
   lift = WebT

derive instance Functor m => Functor (WebT m)

instance Apply m => Apply (WebT m) where
   apply (WebT fs) (WebT xs) = WebT (fs <*> xs)

instance Applicative m => Applicative (WebT m) where
   pure = WebT <<< pure

instance Bind m => Bind (WebT m) where
   bind (WebT x) f = WebT $ x >>= runWebT <<< f

instance Monad m => Monad (WebT m)

instance MonadThrow Error m => MonadThrow Error (WebT m) where
   throwError = lift <<< throwError

instance MonadError Error m => MonadError Error (WebT m) where
   catchError (WebT x) h =
      WebT $ catchError x \e -> runWebT (h e)

instance MonadEffect m => MonadEffect (WebT m) where
   liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (WebT m) where
   liftAff = lift <<< liftAff
