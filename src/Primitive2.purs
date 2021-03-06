module Primitive2 where

import Prelude
import Data.Int (toNumber)
import Data.Either (Either(..))
import Lattice (𝔹, (∧))
import Util (type (+), type (×), (×))

type Op a = a × 𝔹 -> Val 𝔹

data Primitive =
   IntOp (Op Int)

data Val a =
   Int a Int |
   Primitive a Primitive

plus :: Int + Number -> Int + Number -> Int + Number
plus = (+) `union2` (+)

plus' :: (Int + Number) × 𝔹 -> (Int + Number) × 𝔹 -> (Int + Number) × 𝔹
plus' = dependsBoth plus

dependsBoth :: forall a b c . (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹
dependsBoth op (x × α) (y × β) = x `op` y × (α ∧ β)

union2 :: (Int -> Int -> Int) -> (Number -> Number -> Number) -> Int + Number -> Int + Number -> Int + Number
union2 f _ (Left x) (Left y)     = Left (f x y)
union2 _ f (Left x) (Right y)    = Right (f (toNumber x) y)
union2 _ f (Right x) (Right y)   = Right (f x y)
union2 _ f (Right x) (Left y)    = Right (f x (toNumber y))
