module Bindings where

import Prelude
import Data.Either (Either(..))
import Selected (class Lattice, Selected(..), bot, join, meet, top, maybeJoin, maybeMeet, (∧?),
                 (∧), (∨?), (∨))
import Util (error, todo, toBool, (≟))
import Data.Foldable
import Data.Maybe (Maybe(..))

type Var = String

data Bind a = Bind Var a

infix 6 Bind as ↦

data Bindings a =
   Empty | Extend (Bindings a) (Bind a)

infixl 5 Extend as :+:

infixl 5 update as ◃

ε :: ∀ a . Bindings a
ε = Empty

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty = m
   append m1 (Extend m2 kv) = Extend (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = ε

instance bindingsLattice :: Lattice a => Lattice (Bindings a) where
   maybeMeet xs ys          = Just $ intersect xs ys
   meet      xs ys          = intersect xs ys
   maybeJoin xs ys          = Just $ union xs ys
   join      xs ys          = union xs ys
   top       (xs :+: x ↦ v) = top xs :+:  x ↦ top v
   top       Empty          = Empty
   bot       (xs :+: x ↦ v) = bot xs :+:  x ↦ bot v
   bot       Empty          = Empty

foldl :: forall a . (Bind a -> Bindings a -> Bindings a) -> Bindings a -> Bindings a -> Bindings a 
foldl f z (xs :+: Bind v x) = f (Bind v x) (foldl f z xs) 
foldl f z Empty             = z

find :: ∀ a . Var -> Bindings a -> Either String a
find x Empty = Left $ "variable " <> x <> " not found"
find x (m :+: k ↦ v) = if x == k then Right v else find x m

update :: ∀ a . Var -> a -> Bindings a -> Bindings a
update k' v' (xs :+: k ↦ v) 
 = if k == k' then xs :+: k ↦ v' else (update k' v' xs) :+: k ↦ v
update k' v' xs = go ε
   where go (xs :+: k ↦ v) = if k == k' 
                             then    xs :+: k ↦ v' 
                             else go xs :+: k ↦ v
         go Empty = Empty

reverse :: forall a . Bindings a -> Bindings a
reverse = go ε
    where
        go acc (xs :+: x) = go (acc :+: x) xs
        go acc Empty = acc

filter :: forall a. (Bind a -> Boolean) -> Bindings a -> Bindings a
filter p = go Empty
  where
  go acc Empty = reverse acc
  go acc (xs :+: Bind v x)
    | p (Bind v x) = go (acc :+: Bind v x) xs
    | otherwise = go acc xs

any :: forall a. Lattice a => Bind a -> Bindings a -> Boolean
any (Bind v x) (xs :+: Bind v' x') = 
   case v ≟ v', x ∧? x' of 
      Just _, Just _ -> true
      _, _           -> false
any (Bind v x) Empty = false

intersect :: forall a. Lattice a => Bindings a -> Bindings a -> Bindings a
intersect Empty _     = Empty
intersect _     Empty = Empty
intersect xs  ys      = filter (\x -> any x ys) xs

union :: forall a. Lattice a => Bindings a -> Bindings a -> Bindings a
union xs ys =  foldl (deleteBy eq) (nubBy eq ys) xs
   where eq (Bind v x) (Bind v' x') 
          = case v ≟ v', x ∧? x' of 
               Just _, Just _ -> true
               _, _           -> false

deleteBy :: forall a. (Bind a -> Bind a -> Boolean) -> Bind a -> Bindings a -> Bindings a
deleteBy _ _ Empty = Empty
deleteBy eq' x (ys :+: y) | eq' x y = ys
deleteBy eq' x (ys :+: y) = (deleteBy eq' x ys) :+: y

nubBy :: forall a. (Bind a -> Bind a -> Boolean) -> Bindings a -> Bindings a
nubBy _     Empty = Empty
nubBy eq' (xs :+: x) 
 = nubBy eq' (filter (\x' -> not (eq' x x')) xs) :+: x

