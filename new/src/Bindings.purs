module Bindings where

import Prelude hiding (top)
import Data.Either (Either(..))
import Selected
import Util (error, todo, toBool, (≟))
import Data.Foldable
import Data.Eq
import Data.Maybe (Maybe(..))
import Selected (class Lattice, bot, top, (∧?))
import Util (MayFail, (≟))

type Var = String

data Bind a = Bind Var a

infix 6 Bind as ↦

data Bindings a = Empty | Extend (Bindings a) (Bind a)

infixl 5 Extend as :+:

infixl 5 update as ◃

ε :: ∀ a . Bindings a
ε = Empty

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty           = m
   append m1 (Extend m2 kv) = Extend (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = ε

instance bindingsLattice :: Lattice a => Lattice (Bindings a) where
   maybeMeet xs ys          = Just $ intersect xs ys
   maybeJoin xs ys          = Just $ union xs ys
   top       (xs :+: x ↦ v) = top xs :+:  x ↦ top v
   top       Empty          = Empty

   bot       (xs :+: x ↦ v) = bot xs :+:  x ↦ bot v
   bot       Empty          = Empty

instance bindEq :: Lattice a => Eq (Bind a) where
   eq (x ↦ v) (x' ↦ v') = case x ≟ x', v ∧? v' of
                           Just _, Just _ -> true
                           _     , _      -> false

foldl :: forall a . (Bind a -> Bindings a -> Bindings a) -> Bindings a -> Bindings a -> Bindings a
foldl f z (xs :+: x ↦ v) = f (x ↦ v) (foldl f z xs)
foldl f z Empty          = z

find :: forall a . Var -> Bindings a -> Either String a
find x' Empty          = Left $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then Right v else find x' xs

update :: forall a . Lattice a => Var -> a -> Bindings a -> Bindings a
update x' v' (xs :+: x ↦ v)
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update x' v' xs) :+: x ↦ v
update x' v' Empty = ε :+: x' ↦ v'

filter :: forall a. Lattice a => (Bind a -> Boolean) -> Bindings a -> Bindings a
filter p xss = go ε
   where go Empty = ε
         go (xs :+: x ↦ v)
            | p (x ↦ v) = go xs :+: x ↦ v
            | otherwise = go xs

any :: forall a. Lattice a => Bind a -> Bindings a -> Boolean
any (x' ↦ v') (xs :+: x ↦ v)
   | x ↦ v == x' ↦ v' = true
   | otherwise        = any (x' ↦ v') xs
any (x' ↦ v') Empty   = false

intersect :: forall a. Lattice a => Bindings a -> Bindings a -> Bindings a
intersect Empty _     = Empty
intersect _     Empty = Empty
intersect xs  ys      = filter (\x -> any x ys) xs

union :: forall a. Lattice a => Bindings a -> Bindings a -> Bindings a
union xs ys = foldl delete (nub ys) xs

delete :: forall a. Lattice a => Bind a -> Bindings a -> Bindings a
delete _ Empty = Empty
delete x (ys :+: y)
   | x == y    = delete x ys
   | otherwise = delete x ys :+: y

nub :: forall a. Lattice a => Bindings a -> Bindings a
nub Empty            = Empty
nub (xs :+: x ↦ v)   = (nub $ filter (notEq (x ↦ v)) xs) :+: x ↦ v
