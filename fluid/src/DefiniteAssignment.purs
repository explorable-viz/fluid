module DefiniteAssignment where

import Prelude

import Bind (Name, Var)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Foldable (foldl, for_)
import Data.Either (Either)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

type Ctx = Map Var Boolean

-- Class-entry case of the spec context entry θ (Definition 1): ⟨q, x⃗, c⟩.
-- Declaring-context Γ subscript deferred until the unified context exists.
type ClassEntry =
   { mod :: Name
   , base :: Maybe Var -- c: base class (⊥ = Nothing)
   , fields :: List Var -- x⃗: own field names (distinct)
   }

type ClassCtx = Map Var ClassEntry

-- spec context entry θ (Definition 1).
data Entry
   = VarStatus Boolean -- a ∈ 𝔹 (definite-assignment status)
   | Class ClassEntry -- ⟨q, x⃗, c⟩
   | Module Name

-- spec context Γ. (Ctx above is the status-only definite-assignment delta Δ,
-- carried as the AST annotation; it remains separate from Γ for now.)
type Cxt = Map Var Entry

class HasClassCtx m where
   askClassCtx :: m ClassCtx

data TyResult a = Returns | Assigns a

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

-- Class entries of Γ, as a ClassCtx (for the DataType-derived lookups).
classesOf :: Cxt -> ClassCtx
classesOf = Map.mapMaybe case _ of
   Class ce -> Just ce
   _ -> Nothing

-- Override Γ with definite-assignment statuses δ (δ wins).
extendStatuses :: Cxt -> Ctx -> Cxt
extendStatuses γ δ = Map.union (VarStatus <$> δ) γ

-- Inherited then own.
fields :: ClassCtx -> Var -> Either String (List Var)
fields λ = go Set.empty
   where
   go seen c
      | c `Set.member` seen = throwError $ "Cyclic class hierarchy at: " <> c
      | otherwise = case Map.lookup c λ of
           Nothing -> throwError $ "Unknown class: " <> c
           Just { base: Nothing, fields: xs } -> pure xs
           Just { base: Just b, fields: xs } -> (_ <> xs) <$> go (Set.insert c seen) b

unionWith_mergeEq :: ClassCtx -> ClassCtx -> Either String ClassCtx
unionWith_mergeEq a b = do
   let dups = Set.toUnfoldable (Set.intersection (Map.keys a # Set.fromFoldable) (Map.keys b # Set.fromFoldable)) :: List Var
   for_ dups \k -> case Map.lookup k a, Map.lookup k b of
      Just va, Just vb | va /= vb -> throwError $ "Conflicting class declarations: " <> k
      _, _ -> pure unit
   pure (Map.union a b)

-- ======================
-- boilerplate
-- ======================
derive instance Functor TyResult
derive instance Eq a => Eq (TyResult a)

instance (Monad m, HasClassCtx m) => HasClassCtx (StateT s m) where
   askClassCtx = lift askClassCtx

instance (Monad m, HasClassCtx m) => HasClassCtx (ReaderT r m) where
   askClassCtx = lift askClassCtx

instance (Monad m, HasClassCtx m) => HasClassCtx (ExceptT e m) where
   askClassCtx = lift askClassCtx

instance (Monad m, HasClassCtx m, Monoid w) => HasClassCtx (WriterT w m) where
   askClassCtx = lift askClassCtx
