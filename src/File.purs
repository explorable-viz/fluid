module File where

import Prelude

import Data.Newtype (class Newtype)
import Util (AffError)

type FileLoader m = Array Folder -> File -> AffError m String

type FileContext m =
   { loadFile :: FileLoader m
   , fluidSrcPaths :: Array Folder
   }

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
