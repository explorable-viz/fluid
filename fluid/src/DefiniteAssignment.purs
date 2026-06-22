module DefiniteAssignment where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Foldable (foldl, for_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Exception (Error)
import Util (type (×), throw, (×))

type Ctx = Map Var Boolean

type ClassCtx = Map Var (Maybe Var × List Var)

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

-- Mirrors PurePy spec's fields(M.C): inherited then own fields.
fields :: forall m. MonadError Error m => ClassCtx -> Var -> m (List Var)
fields λ = go Set.empty
   where
   go seen c
      | c `Set.member` seen = throw $ "Cyclic class hierarchy at: " <> c
      | otherwise = case Map.lookup c λ of
           Nothing -> throw $ "Unknown class: " <> c
           Just (Nothing × xs) -> pure xs
           Just (Just b × xs) -> (_ <> xs) <$> go (Set.insert c seen) b

unionDisjoint :: forall m. MonadError Error m => ClassCtx -> ClassCtx -> m ClassCtx
unionDisjoint a b = do
   let dups = Set.toUnfoldable (Set.intersection (Map.keys a # Set.fromFoldable) (Map.keys b # Set.fromFoldable)) :: List Var
   for_ dups \k -> case Map.lookup k a, Map.lookup k b of
      Just va, Just vb | va /= vb -> throw $ "Conflicting class declarations: " <> k
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
