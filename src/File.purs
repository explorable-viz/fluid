module File where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Writer (WriterT, lift)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Graph.WithGraph (WithGraphT)

type FileCxt =
   { fluidSrcPaths :: Array Folder
   }

class LoadFile m where
   loadFile :: MonadError Error m => MonadAff m => Array Folder -> File -> m String

instance (Monoid w, MonadError Error m, MonadAff m, LoadFile m) => LoadFile (WriterT w m) where
   loadFile folders file = lift (loadFile folders file)

instance (MonadError Error m, MonadAff m, LoadFile m) => LoadFile (WithGraphT m) where
   loadFile folders file = lift (loadFile folders file)

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
