module WellFormed where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)
import Lattice (Raw)
import SExpr (Module, Stmt) as S

-- Per PurePy spec: well-formedness rules over the surface AST.
-- Trivial first version: always succeeds. Real checks (definite assignment,
-- unreachable code, distinct names in mutual regions, ...) follow.

checkProgram :: forall m. MonadError Error m => Raw S.Stmt -> m Unit
checkProgram = check

checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

-- Shared check over a stmt-shaped thing. Modules currently funnel their
-- own contents through checkModule directly; this hook is where program-
-- and module-level checks meet once they have shared structure.
check :: forall m. MonadError Error m => Raw S.Stmt -> m Unit
check _ = pure unit
