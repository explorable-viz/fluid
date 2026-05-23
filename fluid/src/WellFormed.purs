module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldl, for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, unions)
import Data.Set as Set
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
-- Definite-assignment contexts and result types (PurePy spec §2.1)
-- ======================

-- Context Γ : Var ⇀ B, where B = {tt, ff}. Absent key = ⊥ (undefined).
-- True = definitely assigned (tt); False = not definitely assigned (ff).
type Ctx = Map Var Boolean

-- Well-formedness result type R ::= Returns | Assigns Δ.
data Result = Returns | Assigns Ctx

derive instance Eq Result

-- Sequential composition Γ · Δ on contexts. Right-biased: Δ overrides Γ.
overrideCtx :: Ctx -> Ctx -> Ctx
overrideCtx = flip Map.union

-- Parallel composition Γ ⊕ Δ on contexts.
-- Both defined → conjunction of statuses.
-- Only one defined → ff (the var is "lost" in the merge).
-- Both ⊥ → ⊥.
mergeCtx :: Ctx -> Ctx -> Ctx
mergeCtx γ1 γ2 =
   foldl (\acc k -> Map.insert k (mergedAt k) acc) Map.empty allKeys
   where
   allKeys :: Set Var
   allKeys = Set.fromFoldable (Map.keys γ1) `Set.union` Set.fromFoldable (Map.keys γ2)
   mergedAt k = case Map.lookup k γ1, Map.lookup k γ2 of
      Just a, Just b -> a && b
      _, _ -> false

-- Lifted to Result. Returns is zero for · and unit for ⊕.
overrideRes :: Result -> Result -> Result
overrideRes _ Returns = Returns
overrideRes Returns _ = Returns
overrideRes (Assigns a) (Assigns b) = Assigns (overrideCtx a b)

mergeRes :: Result -> Result -> Result
mergeRes Returns r = r
mergeRes r Returns = r
mergeRes (Assigns a) (Assigns b) = Assigns (mergeCtx a b)

-- ======================
-- Unreachable-statement check
-- ======================
--
-- Uses a Boolean classification (Returns vs not) until the full checkDA
-- refactor lands. The Result type above is in place; consumers come later.

isReturns :: forall a. S.Stmt a -> Boolean
isReturns (S.Return _) = true
isReturns (S.Seq s1 _) = isReturns s1
isReturns (S.If clauses elseBody) =
   all isReturns (snd <$> clauses) && isReturns elseBody
isReturns (S.Match _ branches) =
   all isReturns (snd <$> branches)
isReturns _ = false

check :: forall m a. MonadError Error m => S.Stmt a -> m Unit
check (S.Seq s1 s2)
   | isReturns s1 = throw "Unreachable statement"
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
