module ModuleGraph where

import Data.List (List)
import Data.Map (Map)
import Expr (Module)
import Util (type (×))

type ModuleName = String

-- topsorted × graph × defs
type DependencyGraph' a = List ModuleName × DependencyGraph × Modules a
type DependencyGraph = Map ModuleName (List ModuleName)
type Modules a = Map ModuleName (Module a)
