module Bindings where

import Prelude
import Data.Maybe (Maybe(..))

type Var = String

data Bind a = Bind Var a

data Bindings a =
  Empty | Snoc (Bindings a) (Bind a)

infixl 5 Snoc as :+: -- give this a monoid instance instead

conc :: forall a . Bindings a -> Bindings a -> Bindings a
conc m Empty = m
conc m1 (Snoc m2 kv) = Snoc (conc m1 m2) kv

infixl 5 conc as :++:

find :: forall a . Var -> Bindings a -> Maybe a
find _ Empty = Nothing
find x (Snoc m (Bind k v)) = if x == k then Just v else find x m

derive instance eqBind :: (Eq a) => Eq (Bind a)
instance showBind :: (Show a) => Show (Bind a) where
  show (Bind x a) = "Bind " <> show x <> " " <> show a

derive instance eqBindings :: (Eq a) => Eq (Bindings a)
instance showBindings :: (Show a) => Show (Bindings a) where
  show Empty = "Empty"
  show (Snoc m (Bind k v)) = show m <> ":+: (" <> show k <> ", " <> show v <> ")"
