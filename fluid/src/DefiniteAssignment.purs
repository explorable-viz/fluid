module DefiniteAssignment where

import Prelude

import Bind (Var)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

-- Context Γ : Var ⇀ B, where B = {tt, ff}. Absent key = ⊥ (undefined).
-- True = definitely assigned (tt); False = not definitely assigned (ff).
type Ctx = Map Var Boolean

-- Well-formedness result type R ::= Returns | Assigns Δ.
data TyResult = Returns | Assigns Ctx

derive instance Eq TyResult

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

-- Lifted to TyResult. Returns is zero for · and unit for ⊕.
overrideRes :: TyResult -> TyResult -> TyResult
overrideRes _ Returns = Returns
overrideRes Returns _ = Returns
overrideRes (Assigns a) (Assigns b) = Assigns (overrideCtx a b)

mergeRes :: TyResult -> TyResult -> TyResult
mergeRes Returns r = r
mergeRes r Returns = r
mergeRes (Assigns a) (Assigns b) = Assigns (mergeCtx a b)
