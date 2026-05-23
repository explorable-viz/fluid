module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (all, for_, traverse_)
import Data.List.NonEmpty (NonEmptyList, snoc)
import Data.Maybe (Maybe(..))
import Data.Set (Set, unions)
import Data.Set (singleton) as Set
import Data.String.Common (joinWith)
import Data.Tuple (fst, snd)
import Effect.Exception (Error)
import Expr (bv, fv)
import Expr (Module(..), RecDefs(..)) as E
import Lattice (Raw)
import SExpr (Clause(..), Expr, Module, Stmt(..), VarDef(..)) as S
import Util (isEmpty, throw, (×))
import Util.Map (keys)
import Util.Set ((\\), (∪))
import Val (Env)

-- ======================
-- Entry points
-- ======================

checkProgram :: forall m. MonadError Error m => Set Var -> Raw S.Stmt -> m Unit
checkProgram initialScope s = do
   check s
   checkScope initialScope s

checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

-- Names exported by a desugared module.
moduleExports :: forall a. E.Module a -> Set Var
moduleExports (E.Module ds) = unions (defNames <$> ds)
   where
   defNames (Left vd) = bv vd
   defNames (Right (E.RecDefs _ ρ)) = keys ρ

envNames :: forall a. Env a -> Set Var
envNames = keys

-- ======================
-- Unreachable-statement check
-- ======================

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

-- ======================
-- Variable-in-scope check (PurePy 'var' rule)
-- ======================

checkScope :: forall m a. MonadError Error m => Set Var -> S.Stmt a -> m Unit
checkScope scope (S.Return e) = checkExprScope scope e
checkScope _ S.Pass = pure unit
checkScope scope (S.Def (S.VarDef _ e)) = checkExprScope scope e
checkScope scope (S.DefRec rs) = do
   let regionNames = unions (Set.singleton <<< fst <$> rs)
   let scope' = scope ∪ regionNames
   traverse_ (checkBranch scope') rs
   where
   checkBranch sc (_ × S.Clause (ps × body)) =
      checkScope (sc ∪ unions (bv <$> ps)) body
checkScope scope (S.ExprStmt e) = checkExprScope scope e
checkScope scope (S.Assert cond msg) = do
   checkExprScope scope cond
   case msg of
      Just m -> checkExprScope scope m
      Nothing -> pure unit
checkScope scope (S.Seq s1 s2) = do
   checkScope scope s1
   checkScope (scope ∪ bvBlock s1) s2
checkScope scope (S.If clauses elseBody) = do
   for_ clauses \(cond × body) -> do
      checkExprScope scope cond
      checkScope scope body
   checkScope scope elseBody
checkScope scope (S.Match scrut branches) = do
   checkExprScope scope scrut
   for_ branches \(p × body) ->
      checkScope (scope ∪ bv p) body

checkExprScope :: forall m a. MonadError Error m => Set Var -> S.Expr a -> m Unit
checkExprScope scope e = do
   let unbound = fv e \\ scope
   when (not isEmpty unbound)
      $ throw
      $ "Unbound name: " <> joinWith ", " (Array.fromFoldable unbound)

-- Names a stmt makes visible in its enclosing block (for sequencing).
bvBlock :: forall a. S.Stmt a -> Set Var
bvBlock (S.Def (S.VarDef p _)) = bv p
bvBlock (S.DefRec rs) = unions (Set.singleton <<< fst <$> rs)
bvBlock (S.Seq s1 s2) = bvBlock s1 ∪ bvBlock s2
bvBlock _ = mempty
