module ModuleGraph where

import Prelude

import Data.List (List(..), takeWhile, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map)
import Data.NonEmpty ((:|))
import Bind (Name)

type ModuleName = Name

builtins :: ModuleName
builtins = NonEmptyList ("lib" :| "builtins" : Nil)

prelude :: ModuleName
prelude = NonEmptyList ("lib" :| "prelude" : Nil)

predefined :: List ModuleName
predefined = builtins : prelude : Nil

predefinedDeps :: ModuleName -> List ModuleName
predefinedDeps q = takeWhile (_ /= q) predefined

type DependencyGraph = Map ModuleName (List ModuleName)
