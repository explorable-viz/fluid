module Bindings where

import Prelude hiding (absurd)
import Data.List (List(..), (:), singleton)
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices,
   botOf, definedJoin, expand, maybeJoin
)
import Util (Endo, MayFail, type (×), (×), (≞), (≜), absurd, error, fromJust, report, whenever)

type Var = String -- newtype?

varAnon = "_" :: Var

-- Discrete partial order for variables.
mustGeq :: Var -> Var -> Var
mustGeq x y = fromJust "Must be greater" (whenever (x == y) x)

infixl 4 mustGeq as ⪂

data Binding t a = Binding Var (t a)
data Bindings t a = Empty | Extend (Bindings t a) (Binding t a)

infix 6 Binding as ↦
infixl 5 Extend as :+:
infixl 5 update as ◃

find :: forall t a . Var -> Bindings t a -> MayFail (t a)
find x Empty  = report $ "variable " <> x <> " not found"
find x (ρ :+: x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x ρ

foldEnv :: forall t a b . (Binding t a -> Endo b) -> b -> Bindings t a -> b
foldEnv f z (ρ :+: x ↦ v)   = f (x ↦ v) $ foldEnv f z ρ
foldEnv _ z Empty           = z

update :: forall t a . Bindings t a -> Binding t a -> Bindings t a
update Empty _ = Empty
update (ρ :+: x ↦ v) (x' ↦ v')
   | x == x'    = ρ :+: x' ↦ v'
   | otherwise  = update ρ (x' ↦ v') :+: x ↦ v

splitAt :: forall t a . Int -> Bindings t a -> Bindings t a × Bindings t a
splitAt n ρ
  | n <= 0     = ρ × Empty
  | otherwise  = splitAt' n ρ
   where
   splitAt' :: Int -> Bindings t a -> Bindings t a × Bindings t a
   splitAt' _  Empty        = Empty × Empty
   splitAt' 1  (ρ0 :+: xv)  = ρ0 × Extend Empty xv
   splitAt' m  (ρ0 :+: xv)  = ρ' × (ρ'' :+: xv)
      where
      ρ' × ρ'' = splitAt' (m - 1) ρ0

length :: forall t a . Bindings t a -> Int
length Empty      = 0
length (ρ :+: _)  = 1 + length ρ

fromList :: forall t a . List (Binding t a) -> Bindings t a
fromList Nil      = Empty
fromList (xv : ρ) = fromList ρ :+: xv

toList :: forall t a . Bindings t a -> List (Binding t a)
toList Empty      = Nil
toList (ρ :+: xv) = toList ρ <> singleton xv

-- ======================
-- boilerplate
-- ======================
derive instance functorBinding :: Functor t => Functor (Binding t)
derive instance functorBindings :: Functor t => Functor (Bindings t)

instance semigroupBindings :: Semigroup (Bindings t a) where
   append ρ Empty          = ρ
   append ρ (Extend ρ' xv) = Extend (append ρ ρ') xv

instance monoidBindings :: Monoid (Bindings t a) where
   mempty = Empty

instance joinSemilatticeBindings :: Slices (t a) => JoinSemilattice (Bindings t a) where
   join = definedJoin

instance slicesBindings :: Slices (t a) => Slices (Bindings t a) where
   maybeJoin Empty Empty                     = pure Empty
   maybeJoin (ρ :+: x ↦ v) (ρ' :+: y ↦ v')   = (:+:) <$> maybeJoin ρ ρ' <*> ((↦) <$> (x ≞ y) <*> maybeJoin v v')
   maybeJoin _ _                             = report "Bindings of different lengths"

instance boundedSlices :: BoundedSlices (t Boolean) => BoundedSlices (Bindings t Boolean) where
   botOf Empty = Empty
   botOf (Extend ρ (x ↦ v)) = Extend (botOf ρ) (x ↦ botOf v)

instance expandableBindings :: Expandable (t a) => Expandable (Bindings t a) where
   expand Empty Empty                              = Empty
   expand (Extend ρ (x ↦ v)) (Extend ρ' (x' ↦ v')) = Extend (expand ρ ρ') ((x ≜ x') ↦ expand v v')
   expand _ _                                      = error absurd
