module WellFormed where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (all, traverse_)
import Data.List.NonEmpty (NonEmptyList, snoc)
import Data.Tuple (snd)
import Effect.Exception (Error)
import Lattice (Raw)
import SExpr (Clause(..), Module, Stmt(..)) as S
import Util (throw, (×))

checkProgram :: forall m. MonadError Error m => Raw S.Stmt -> m Unit
checkProgram = check

checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

data Result = TyReturns | TyAssigns

derive instance Eq Result

resultType :: forall a. S.Stmt a -> Result
resultType (S.Return _) = TyReturns
resultType S.Pass = TyAssigns
resultType (S.Def _) = TyAssigns
resultType (S.DefRec _) = TyAssigns
resultType (S.ExprStmt _) = TyAssigns
resultType (S.Assert _ _) = TyAssigns
resultType (S.Seq s1 s2) = case resultType s1 of
   TyReturns -> TyReturns
   TyAssigns -> resultType s2
resultType (S.If clauses elseBody) = merge (snoc (snd <$> clauses <#> resultType) (resultType elseBody))
resultType (S.Match _ branches) = merge (branches <#> snd <#> resultType)

-- Returns is identity for branch merge (⨅): all-Returns ⇒ Returns, else Assigns.
merge :: NonEmptyList Result -> Result
merge ts = if all (_ == TyReturns) ts then TyReturns else TyAssigns

check :: forall m a. MonadError Error m => S.Stmt a -> m Unit
check (S.Seq s1 s2)
   | resultType s1 == TyReturns = throw "Unreachable statement"
   | otherwise = check s1 *> check s2
check (S.If clauses elseBody) =
   traverse_ (check <<< snd) clauses *> check elseBody
check (S.Match _ branches) =
   traverse_ (check <<< snd) branches
check (S.DefRec defs) = traverse_ checkBranch defs
   where
   checkBranch (_ × S.Clause (_ × body)) = check body
check _ = pure unit
