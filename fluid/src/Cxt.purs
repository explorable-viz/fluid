module Cxt where

import Prelude hiding (absurd)

import Bind (Name, Var, dottedName)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.List.NonEmpty (unsnoc)
import Data.List.NonEmpty as NEL
import Data.List (List)
import Data.Map (Map)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Types (Scheme(..), Type, fv) as T
import Util (type (+), absurd, error)
import Util.Map (intersectionWith, lookup, unionWith)
import Util.Set ((∪))

data Entry
   = Unbound -- declared later in scope
   | Declared T.Type -- definitely unassigned
   | PossiblyUnassigned (T.Type + T.Scheme)
   | Assigned T.Type
   | TypeScheme T.Scheme
   | Type Name -- class, by fully-qualified name
   | Mod Name
   | ModChecked Name Cxt
   | PredefName
   | TypeVar -- type parameter
   | TypeAlias (List Var) T.Type

type Cxt = Map Var Entry

data WfResult a = Returns | Assigns a

extend :: Cxt -> Cxt -> Cxt
extend cxt cxt' = unionWith extendEntry cxt cxt'

extendEntry :: Entry -> Entry -> Entry
extendEntry (ModChecked q cxt) (ModChecked q' cxt') | q == q' = ModChecked q (cxt `extend` cxt')
extendEntry (Mod q) θ'@(ModChecked q' _) | q == q' = θ'
extendEntry θ@(ModChecked q _) (Mod q') | q == q' = θ
extendEntry _ θ' = θ'

class Override a where
   override :: a -> a -> a

instance Override (Map Var Entry) where
   override = flip (∪)

instance Override a => Override (WfResult a) where
   override _ Returns = Returns
   override Returns _ = Returns
   override (Assigns cxt) (Assigns cxt') = Assigns (cxt `override` cxt')

class Merge a where
   merge :: a -> a -> a

instance Merge (Map Var Entry) where
   merge cxt cxt' = intersectionWith mergeEntry cxt cxt' ∪ (possiblyUnassigned <$> (cxt ∪ cxt'))
      where
      mergeEntry θ θ' = if θ == θ' then θ else possiblyUnassigned θ

      possiblyUnassigned (Declared τ) = PossiblyUnassigned (Left τ)
      possiblyUnassigned (PossiblyUnassigned π) = PossiblyUnassigned π
      possiblyUnassigned (Assigned τ) = PossiblyUnassigned (Left τ)
      possiblyUnassigned (TypeScheme π) = PossiblyUnassigned (Right π)
      possiblyUnassigned _ = error absurd

instance Merge a => Merge (WfResult a) where
   merge Returns r = r
   merge r Returns = r
   merge (Assigns cxt) (Assigns cxt') = Assigns (cxt `merge` cxt')

classOf :: forall m. MonadThrow String m => Cxt -> Name -> m Name
classOf cxt c = case resolveName cxt c of
   Just (Type c') -> pure c'
   _ -> throwError $ "Unknown dataclass: " <> dottedName c

resolveName :: Cxt -> Name -> Maybe Entry
resolveName cxt name = case NEL.fromList init of
   Nothing -> simpleEntry cxt x
   Just q -> case resolveName cxt q of
      Just (ModChecked _ cxt') -> simpleEntry cxt' x
      _ -> Nothing
   where
   { init, last: x } = unsnoc name
   simpleEntry g y = case lookup y g of
      Just e@(Assigned _) -> Just e
      Just e@(TypeScheme _) -> Just e
      Just e@(ModChecked _ _) -> Just e
      Just e@(Type _) -> Just e
      Just e@TypeVar -> Just e
      Just e@(TypeAlias _ _) -> Just e
      _ -> Nothing

-- ======================
-- boilerplate
-- ======================
derive instance Functor WfResult
derive instance Eq a => Eq (WfResult a)
derive instance Eq Entry

generalise :: T.Type -> Entry
generalise τ = maybe (Assigned τ) (TypeScheme <<< flip T.Forall τ) (NEL.fromFoldable (T.fv τ))
