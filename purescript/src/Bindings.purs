module Bindings where

import Prelude
import Data.Maybe (Maybe(..))

type Var = String

data Bind a = Bind Var a

infix 6 Bind as ↦

data Bindings a =
   Empty | Snoc (Bindings a) (Bind a)

infixl 5 Snoc as :+:

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty = m
   append m1 (Snoc m2 kv) = Snoc (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = Empty

find :: forall a . Var -> Bindings a -> Maybe a
find _ Empty = Nothing
find x (Snoc m (k ↦ v)) = if x == k then Just v else find x m

derive instance eqBind :: (Eq a) => Eq (Bind a)
derive instance eqBindings :: (Eq a) => Eq (Bindings a)
