module ModuleGraph where

import Prelude

import Data.List (List(..), takeWhile, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map)
import Data.NonEmpty ((:|))
import Bind (Name)
import DataType (ClassTable)
import Expr (Module)

type ModuleName = Name

builtins :: ModuleName
builtins = NonEmptyList ("lib" :| "builtins" : Nil)

prelude :: ModuleName
prelude = NonEmptyList ("lib" :| "prelude" : Nil)

predefined :: List ModuleName
predefined = builtins : prelude : Nil

predefinedDeps :: ModuleName -> List ModuleName
predefinedDeps q = takeWhile (_ /= q) predefined

type ModuleCxt a =
   { roots :: List ModuleName
   , graph :: DependencyGraph
   , modules :: Modules a
   , classes :: ClassTable
   }

type DependencyGraph = Map ModuleName (List ModuleName)
type Modules a = Map ModuleName (Module a)
