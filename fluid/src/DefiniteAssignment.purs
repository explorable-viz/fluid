module DefiniteAssignment where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Exception (Error)
import Util (type (×), throw, (×))

type Ctx = Map Var Boolean

-- Class context: class name ↦ (optional base class, own field names in declaration order).
type ClassCtx = Map Var (Maybe Var × List Var)

data TyResult a = Returns | Assigns a

derive instance Functor TyResult
derive instance Eq a => Eq (TyResult a)

overrideCtx :: Ctx -> Ctx -> Ctx
overrideCtx = flip Map.union

mergeCtx :: Ctx -> Ctx -> Ctx
mergeCtx γ1 γ2 =
   foldl (\acc k -> Map.insert k (mergedAt k) acc) Map.empty allKeys
   where
   allKeys :: Set Var
   allKeys = Set.fromFoldable (Map.keys γ1) `Set.union` Set.fromFoldable (Map.keys γ2)
   mergedAt k = case Map.lookup k γ1, Map.lookup k γ2 of
      Just a, Just b -> a && b
      _, _ -> false

overrideRes :: TyResult Ctx -> TyResult Ctx -> TyResult Ctx
overrideRes _ Returns = Returns
overrideRes Returns _ = Returns
overrideRes (Assigns a) (Assigns b) = Assigns (overrideCtx a b)

mergeRes :: TyResult Ctx -> TyResult Ctx -> TyResult Ctx
mergeRes Returns r = r
mergeRes r Returns = r
mergeRes (Assigns a) (Assigns b) = Assigns (mergeCtx a b)

-- Inherited then own fields, mirroring spec's fields(M.C). Throws on undefined base or cycle.
fields :: forall m. MonadError Error m => ClassCtx -> Var -> m (List Var)
fields λ = go Set.empty
   where
   go seen c
      | c `Set.member` seen = throw $ "Cyclic class hierarchy at: " <> c
      | otherwise = case Map.lookup c λ of
           Nothing -> throw $ "Unknown class: " <> c
           Just (Nothing × xs) -> pure xs
           Just (Just b × xs) -> (_ <> xs) <$> go (Set.insert c seen) b
