module Bindings where

import Prelude
import Data.List (List(..), (:))
import Data.Set (Set, empty, singleton, union)
import Data.Tuple (Tuple(..), fst, snd)
import Util (type (×), definitely, whenever)

type Var = String

varAnon = "_" :: Var

-- Discrete partial order for variables.
mustGeq :: Var -> Var -> Var
mustGeq x y = definitely "greater" (whenever (x == y) x)

type Bind a = Var × a

key :: forall a . Bind a -> Var
key = fst

val :: forall a . Bind a -> a
val = snd

keys :: forall a . List (Bind a) -> Set Var
keys Nil           = empty
keys (x ↦ _ : ρ)   = singleton x `union` keys ρ

infix 7 Tuple as ↦
infixl 4 mustGeq as ⪂
