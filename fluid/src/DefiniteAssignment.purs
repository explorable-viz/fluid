module DefiniteAssignment where

import Prelude

import Bind (Name, Var)
import Data.Foldable (foldl)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Util (definitely')

type VarCxt = Map Var Boolean

type ClassEntry =
   { cxt :: Cxt -- declaring context (resolves the base class)
   , name :: Name -- fully-qualified name, the class's identity
   , base :: Maybe Var -- base class, if any
   , fields :: List Var -- own field names, distinct
   }

data Entry
   = VarStatus Boolean -- definite-assignment status
   | Class ClassEntry
   | Mod Name
   | ModLoaded Name Cxt

type Cxt = Map Var Entry

data WfResult a = Returns | Assigns a

extendCxtWith :: Cxt -> Cxt -> Cxt
extendCxtWith γ γ' = Map.unionWith extendEntry γ γ'

extendEntry :: Entry -> Entry -> Entry
extendEntry (ModLoaded q γ) (ModLoaded q' γ') | q == q' = ModLoaded q (γ `extendCxtWith` γ')
extendEntry (Mod q) θ'@(ModLoaded q' _) | q == q' = θ'
extendEntry θ@(ModLoaded q _) (Mod q') | q == q' = θ
extendEntry _ θ' = θ'

overrideVarCxt :: VarCxt -> VarCxt -> VarCxt
overrideVarCxt = flip Map.union

mergeVarCxt :: VarCxt -> VarCxt -> VarCxt
mergeVarCxt γ1 γ2 =
   foldl (\acc k -> Map.insert k (mergedAt k) acc) Map.empty allKeys
   where
   allKeys :: Set Var
   allKeys = Set.fromFoldable (Map.keys γ1) `Set.union` Set.fromFoldable (Map.keys γ2)
   mergedAt k = case Map.lookup k γ1, Map.lookup k γ2 of
      Just a, Just b -> a && b
      _, _ -> false

overrideRes :: WfResult VarCxt -> WfResult VarCxt -> WfResult VarCxt
overrideRes _ Returns = Returns
overrideRes Returns _ = Returns
overrideRes (Assigns a) (Assigns b) = Assigns (overrideVarCxt a b)

mergeRes :: WfResult VarCxt -> WfResult VarCxt -> WfResult VarCxt
mergeRes Returns r = r
mergeRes r Returns = r
mergeRes (Assigns a) (Assigns b) = Assigns (mergeVarCxt a b)

-- The desugared program's context: module and class entries resolved away.
erase :: Cxt -> VarCxt
erase = Map.mapMaybe case _ of
   VarStatus b -> Just b
   _ -> Nothing

classFor :: Cxt -> Var -> Maybe ClassEntry
classFor γ c = case Map.lookup c γ of
   Just (Class cls) -> Just cls
   _ -> Nothing

extendCxt :: Cxt -> VarCxt -> Cxt
extendCxt γ δ = Map.union (VarStatus <$> δ) γ

fields :: ClassEntry -> List Var
fields cls = case cls.base of
   Nothing -> cls.fields
   Just b -> fields (definitely' (classFor cls.cxt b)) <> cls.fields

-- ======================
-- boilerplate
-- ======================
derive instance Functor WfResult
derive instance Eq a => Eq (WfResult a)
derive instance Eq Entry

