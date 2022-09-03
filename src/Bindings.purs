module Bindings where

import Prelude
import Data.List (List(..), (:))
import Data.Set (Set, empty, singleton, union)
import Data.Tuple (Tuple(..), fst, snd)
import Util (MayFail, type (×), definitely, report, whenever)

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

dom :: forall a . List (Bind a) -> Set Var
dom Nil           = empty
dom (x ↦ _ : ρ)   = singleton x `union` dom ρ

infix 7 Tuple as ↦
infixl 4 mustGeq as ⪂

find :: forall a . Var -> List (Bind a) -> MayFail a
find x Nil  = report ("variable " <> x <> " not found")
find x (x' ↦ v : ρ)
   | x == x'   = pure v
   | otherwise = find x ρ
