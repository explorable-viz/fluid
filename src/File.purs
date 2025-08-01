module File where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT, lift)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Util (AffError)

newtype FileCxt = FileCxt { fluidSrcPaths :: Array Folder }

class LoadFile m where
   loadFileFromPaths :: MonadError Error m => MonadAff m => Array File -> m String

instance (Monoid w, MonadError Error m, MonadAff m, LoadFile m) => LoadFile (WriterT w m) where
   loadFileFromPaths = lift <<< loadFileFromPaths

instance (MonadAff m, MonadError Error m, LoadFile m) => LoadFile (StateT s m) where
   loadFileFromPaths = lift <<< loadFileFromPaths

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

loadFile :: forall m. LoadFile m => Array Folder -> File -> AffError m String
loadFile folders (File file) = loadFileFromPaths paths
   where
   paths = flip prependFolder (File $ file <> fluidExtension) <$> folders
