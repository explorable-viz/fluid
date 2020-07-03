module Env where

import Prelude hiding (top, absurd)
import Lattice (class Lattice, class Selectable, mapα, maybeZipWithα)
import Val (Val)

type Var = String

varAnon = "_" :: Var

data Bind = Bind Var Val
data Env = Empty | Extend Env Bind

infix 6 Bind as ↦
infixl 5 Extend as :+:

instance semigroupEnv :: Semigroup Env where
   append m Empty          = m
   append m (Extend m' kv) = Extend (append m m') kv

instance bindingsMonoid :: Monoid Env where
   mempty = Empty
