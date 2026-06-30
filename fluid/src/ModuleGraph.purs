module ModuleGraph where

import Data.List (List)
import Data.Map (Map)
import Bind (Name)
import DefiniteAssignment (Cxt)
import Expr (Module)

type ModuleName = Name

type ModuleCxt a =
   { roots :: List ModuleName
   , graph :: DependencyGraph
   , modules :: Modules a
   , classCtx :: Cxt
   }

type DependencyGraph = Map ModuleName (List ModuleName)
type Modules a = Map ModuleName (Module a)
