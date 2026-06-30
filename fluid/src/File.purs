module File where

import Prelude

import Affjax (Error(..)) as A
import Affjax (Response)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (defaultRequest, request)
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT, lift)
import Data.Array (foldM)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import DefiniteAssignment (Cxt)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Util (type (×), (×), debug, error)

newtype FileCxt = FileCxt { fluidSrcPaths :: Array Folder, classCtx :: Cxt }

class LoadFile m where
   loadFileFromPath :: MonadError Error m => MonadAff m => File -> m (Maybe String)

instance (Monoid w, MonadError Error m, MonadAff m, LoadFile m) => LoadFile (WriterT w m) where
   loadFileFromPath = lift <<< loadFileFromPath

instance (MonadAff m, MonadError Error m, LoadFile m) => LoadFile (StateT s m) where
   loadFileFromPath = lift <<< loadFileFromPath

instance LoadFile Aff where
   loadFileFromPath (File path) = do
      result <- runExceptT $ do
         resp × path' <- ExceptT $ liftAff $ requestPath
         when debug.logging $ liftAff $ log ("loadFileFromPath: resolved path: " <> path')
         pure resp.body
      pure $ either (const Nothing) Just result
      where
      requestPath :: Aff (Either A.Error (Response String × String))
      requestPath = do
         resp <- request (defaultRequest { url = path, method = Left GET, responseFormat = string })
         pure case resp of
            Right resp' | resp'.status == StatusCode 200 -> Right (resp' × path)
            Right _ -> Left A.RequestFailedError
            Left err -> Left err

newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File
derive instance Newtype Folder _
derive newtype instance Show Folder

instance Semigroup Folder where
   append (Folder folder1) (Folder folder2) = Folder (folder1 <> "/" <> folder2)

prependFolder :: Folder -> File -> File
prependFolder (Folder folder) (File file) = File (folder <> "/" <> file)

infixr 5 prependFolder as </>

fluidExtension :: String
fluidExtension = ".fld"

loadFile :: forall m. LoadFile m => Monad m => MonadError Error m => MonadAff m => Array Folder -> File -> m String
loadFile folders file = do
   let paths = prependFolder <$> folders <*> [ file ]
   result <- foldM step Nothing paths
   case result of
      Just contents -> pure contents
      Nothing -> error ("File not found in any path: " <> show paths)
   where
   step :: Maybe String -> File -> m (Maybe String)
   step (Just contents) _ = pure (Just contents)
   step Nothing path = loadFileFromPath path
