module Module.Files where

import Prelude

import Data.Newtype (class Newtype)
import Util (AffError)

type FileLoader = forall m. Folder -> File -> AffError m String

newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File