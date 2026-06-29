module ModuleGraph where

import Data.List (List)
import Data.Map (Map)
import Bind (Name)
import DefiniteAssignment (ClassCtx)
import Expr (Module)

type ModuleName = Name

type ModuleCxt a =
   { roots :: List ModuleName
   , graph :: DependencyGraph
   , modules :: Modules a
   , classCtx :: ClassCtx
   }

type DependencyGraph = Map ModuleName (List ModuleName)
type Modules a = Map ModuleName (Module a)
