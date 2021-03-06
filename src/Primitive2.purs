module Primitive2 where

import Prelude hiding (absurd, apply)
import Lattice (𝔹, (∧))
import Util (type (×), (×), absurd, error)

type Op a = a × 𝔹 -> Val 𝔹

data Primitive =
   IntOp (Op Int)

data Val a =
   Int a Int |
   Primitive Primitive

instance showVal :: Show (Val Boolean) where
   show (Int α n)       = show n <> "_" <> show α
   show (Primitive op)  = error "todo"

class To a where
   to :: Val 𝔹 -> a × 𝔹

class From a where
   from :: a × 𝔹 -> Val 𝔹

getα :: Val 𝔹 -> 𝔹
getα (Int α _) = α
getα _         = error absurd

instance toInt :: To Int where
   to (Int α n)   = n × α
   to _           = error "Int expected"

instance fromInt :: From Int where
   from (n × α) = Int α n

from1 :: forall a . From a => (Int × 𝔹 -> a × 𝔹) -> Val 𝔹
from1 op = Primitive (IntOp (op >>> from))

from2 :: (Int × 𝔹 -> Int × 𝔹 -> Int × 𝔹) -> Val 𝔹
from2 op = Primitive (IntOp (op >>> from1))

apply :: Primitive -> Val 𝔹 -> Val 𝔹
apply (IntOp op) v = op (to v)

apply' :: Val 𝔹 -> Val 𝔹 -> Val 𝔹
apply' (Primitive op)   = apply op
apply' _                = error absurd

plus_ :: Val 𝔹
plus_ = from2 plus

plus :: Int × 𝔹 -> Int × 𝔹 -> Int × 𝔹
plus = dependsBoth (+)

dependsBoth :: forall a b c . (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹
dependsBoth op (x × α) (y × β) = x `op` y × (α ∧ β)

testPrim :: Val 𝔹
testPrim = apply' (apply' plus_ (Int true 5)) (Int true 6)
