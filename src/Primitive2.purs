module Primitive2 where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..))
import Data.Tuple (snd)
import Debug.Trace (trace)
import Math (log, pow)
import Bindings (Bindings(..), (:+:), (↦))
import DataType (Ctr, cCons)
import Lattice (𝔹, (∧))
import Util (type (×), (×), type (+), absurd, error)

type Op a = a × 𝔹 -> Val 𝔹

data Val a =
   Int a Int |
   Constr a Ctr (List (Val a)) |
   Primitive (Val 𝔹 -> Val 𝔹)

instance showVal :: Show (Val Boolean) where
   show (Int α n)       = show n <> "_" <> show α
   show (Constr _ _ _)  = error "todo"
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

from1 :: forall a b . To a => From b => (a × 𝔹 -> b × 𝔹) -> Val 𝔹
from1 op = Primitive (to >>> op >>> from)

from2 :: forall a b c . To a => To b => From c => (a × 𝔹 -> b × 𝔹 -> c × 𝔹) -> Val 𝔹
from2 op = Primitive (to >>> op >>> from1)

apply :: Val 𝔹 -> Val 𝔹 -> Val 𝔹
apply (Primitive op)   = op
apply _                = error absurd

plus_ :: Val 𝔹
plus_ = from2 plus

plus :: (Int + Number) × 𝔹 -> (Int + Number) × 𝔹 -> (Int + Number) × 𝔹
plus = dependsBoth ((+) `union2` (+))

times_ :: Val 𝔹
times_ = from2 times

times :: Int × 𝔹 -> Int × 𝔹 -> Int × 𝔹
times = dependsNonZero (*)

dependsBoth :: forall a b c . (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹
dependsBoth op (x × α) (y × β) = x `op` y × (α ∧ β)

class DependsBinary a b c where
   dependsNonZero :: (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹

-- If both are false, we depend on the first.
instance dependsNonZeroIntInt :: DependsBinary Int Int a where
   dependsNonZero op (x × α) (y × β) =
      x `op` y × if x == 0 then α else if y == 0 then β else α ∧ β

primitives :: Bindings Val 𝔹
primitives = foldl (:+:) Empty [
   -- some signatures are specified for clarity or to drive instance resolution
   -- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
   "+"         ↦ from   ((+) `union2` (+)),
   "-"         ↦ from   ((-) `union2` (-)),
   "*"         ↦ from   ((*) `union2` (*)),
   "**"        ↦ from   ((\x y -> toNumber x `pow` toNumber y) `union2'` pow),
   "/"         ↦ from   ((\x y -> toNumber x / toNumber y)  `union2'` (/)),
   "=="        ↦ from   ((==) `union2'` (==) `unionDisj` (==)),
   "/="        ↦ from   ((/=) `union2'` (/=) `unionDisj` (==)),
   "<"         ↦ from   ((<)  `union2'` (<)  `unionDisj` (==)),
   ">"         ↦ from   ((>)  `union2'` (>)  `unionDisj` (==)),
   "<="        ↦ from   ((<=) `union2'` (<=) `unionDisj` (==)),
   ">="        ↦ from   ((>=) `union2'` (>=) `unionDisj` (==)),
   "++"        ↦ from   ((<>) :: String -> String -> String),
   ":"         ↦ Constr false cCons Nil,
--   "!"         ↦ from   matrixLookup,
   "ceiling"   ↦ from   ceil,
   "debugLog"  ↦ from   debugLog,
   "dims"      ↦ from   (snd :: Array (Array (Val 𝔹)) × (Int × Int) -> Int × Int),
   "div"       ↦ from   (div :: Int -> Int -> Int),
   "error"     ↦ from   (error :: String -> Boolean),
   "floor"     ↦ from   floor,
   "log"       ↦ from   ((toNumber >>> log) `union` log),
   "numToStr"  ↦ from   (show `union` show)
]

debugLog :: Val 𝔹 -> Val 𝔹
debugLog x = trace x (const x)

-- Could improve this a bit with some type class shenanigans, but not straightforward.
union :: forall a . (Int -> a) -> (Number -> a) -> Int + Number -> a
union f _ (Left x)   = f x
union _ f (Right x)  = f x

union2 :: (Int -> Int -> Int) -> (Number -> Number -> Number) -> Int + Number -> Int + Number -> Int + Number
union2 f _ (Left x) (Left y)     = Left $ f x y
union2 _ f (Left x) (Right y)    = Right $ f (toNumber x) y
union2 _ f (Right x) (Right y)   = Right $ f x y
union2 _ f (Right x) (Left y)    = Right $ f x (toNumber y)

union2' :: forall a . (Int -> Int -> a) -> (Number -> Number -> a) -> Int + Number -> Int + Number -> a
union2' f _ (Left x) (Left y)    = f x y
union2' _ f (Left x) (Right y)   = f (toNumber x) y
union2' _ f (Right x) (Right y)  = f x y
union2' _ f (Right x) (Left y)   = f x (toNumber y)

unionDisj :: forall a b . (b -> b -> a) -> (String -> String -> a) -> b + String -> b + String -> a
unionDisj f _ (Left x) (Left y)   = f x y
unionDisj _ _ (Left _) (Right _)  = error "Non-uniform argument types"
unionDisj _ f (Right x) (Right y) = f x y
unionDisj _ _ (Right _) (Left _)  = error "Non-uniform argument types"

testPrim :: Val 𝔹
testPrim = apply (apply times_ (Int false 0)) (Int true 0)
