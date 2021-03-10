module Primitive2 where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable)
import Data.Profunctor.Strong (first)
import Debug.Trace (trace)
import Math (log, pow)
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), Var, (:+:), (↦))
import DataType (cCons, cFalse, cPair, cTrue)
import Lattice (𝔹, (∧))
import Util (type (×), (×), type (+), (!), absurd, dup, error)
import Val2 (MatrixRep, Val(..), getα, setα)

-- name in user land, precedence 0 from 9 (similar from Haskell 98), associativity
type OpDef = {
   op    :: Var,
   prec  :: Int,
   assoc :: Assoc
}

opDef :: Var -> Int -> Assoc -> Var × OpDef
opDef op prec assoc = op × { op, prec, assoc }

-- Syntactic information only. No guarantee that any of these will be defined.
opDefs :: Map String OpDef
opDefs = fromFoldable [
   opDef "!"   8 AssocLeft,
   opDef "**"  8 AssocRight,
   opDef "*"   7 AssocLeft,
   opDef "/"   7 AssocLeft,
   opDef "+"   6 AssocLeft,
   opDef "-"   6 AssocLeft,
   opDef ":"   6 AssocRight,
   opDef "++"  5 AssocRight,
   opDef "=="  4 AssocNone,
   opDef "/="  4 AssocNone,
   opDef "<"   4 AssocLeft,
   opDef ">"   4 AssocLeft,
   opDef "<="  4 AssocLeft,
   opDef ">="  4 AssocLeft
]

class From a where
   from :: Val 𝔹 -> a × 𝔹

class To a where
   to :: a × 𝔹 -> Val 𝔹

instance toVal :: From (Val Boolean) where
   from v = v × getα v

instance fromVal :: To (Val Boolean) where
   to (v × α) = setα α v

instance toInt :: From Int where
   from (Int α n)   = n × α
   from _           = error "Int expected"

instance fromInt :: To Int where
   to (n × α) = Int α n

instance toNumber :: From Number where
   from (Float α n) = n × α
   from _           = error "Float expected"

instance fromNumber :: To Number where
   to (n × α) = Float α n

instance toString :: From String where
   from (Str α str) = str × α
   from _           = error "Str expected"

instance fromString :: To String where
   to (str × α) = Str α str

instance toIntOrNumber :: From (Int + Number) where
   from (Int α n)    = Left n × α
   from (Float α n)  = Right n × α
   from _            = error "Int or Float expected"

instance fromIntOrNumber :: To (Int + Number) where
   to (Left n × α)    = Int α n
   to (Right n × α)   = Float α n

instance toIntOrNumberOrString :: From (Either (Either Int Number) String) where
   from (Int α n)   = Left (Left n) × α
   from (Float α n) = Left (Right n) × α
   from (Str α n)   = Right n × α
   from _           = error "Int, Float or Str expected"

instance toIntAndInt :: From (Int × Boolean × (Int × Boolean)) where
   from (Constr α c (v : v' : Nil)) | c == cPair  = from v × from v' × α
   from _                                         = error "Pair expected"

instance toMatrixRep :: From (Array (Array (Val Boolean)) × (Int × Boolean) × (Int × Boolean)) where
   from (Matrix α (vss × i × j))   = vss × i × j × α
   from _                          = error "Matrix expected"

instance fromPair :: To (Val Boolean × Val Boolean) where
   to (v × v' × α) = Constr α cPair (v : v' : Nil)

unary :: forall a b . From a => To b => (a × 𝔹 -> b × 𝔹) -> Val 𝔹
unary op = Primitive (from >>> op >>> to)

binary :: forall a b c . From a => From b => To c => (a × 𝔹 -> b × 𝔹 -> c × 𝔹) -> Val 𝔹
binary op = Primitive (from >>> op >>> unary)

apply :: Val 𝔹 -> Val 𝔹 -> Val 𝔹
apply (Primitive op)   = op
apply _                = error absurd

depends :: forall a b . (a -> b) -> a × 𝔹 -> b × 𝔹
depends = first

depends_bwd :: 𝔹 -> 𝔹
depends_bwd = identity

dependsBoth :: forall a b c . (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹
dependsBoth op (x × α) (y × β) = x `op` y × (α ∧ β)

dependsBoth_bwd :: 𝔹 -> 𝔹 × 𝔹
dependsBoth_bwd = dup

dependsNeither :: forall a b c . (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹
dependsNeither op (x × _) (y × _) = x `op` y × true

dependsNeither_bwd :: 𝔹 -> 𝔹 × 𝔹
dependsNeither_bwd _ = false × false

class DependsBinary a b c where
   dependsNonZero :: (a -> b -> c) -> a × 𝔹 -> b × 𝔹 -> c × 𝔹

-- If both are false, we depend on the first.
instance dependsNonZeroInt :: DependsBinary Int Int a where
   dependsNonZero op (x × α) (y × β) =
      x `op` y × if x == 0 then α else if y == 0 then β else α ∧ β

instance dependsNonZeroNumber :: DependsBinary Number Number a where
   dependsNonZero op (x × α) (y × β) =
      x `op` y × if x == 0.0 then α else if y == 0.0 then β else α ∧ β

instance dependsNonZeroIntOrNumber :: DependsBinary (Int + Number) (Int + Number) a where
   dependsNonZero op (x × α) (y × β) =
      x `op` y ×
      if x `((==) `union2'` (==))` (Left 0)
      then α
      else if y `((==) `union2'` (==))` (Left 0) then β else α ∧ β

instance fromBoolean :: To Boolean where
   to (true × α)   = Constr α cTrue Nil
   to (false × α)  = Constr α cFalse Nil

primitives :: Bindings Val 𝔹
primitives = foldl (:+:) Empty [
   -- some signatures are specified for clarity or from drive instance resolution
   -- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
   "+"         ↦ binary (dependsBoth ((+) `union2` (+))),
   "-"         ↦ binary (dependsBoth ((-) `union2` (-))),
   "*"         ↦ binary (dependsNonZero ((*) `union2` (*))),
   "**"        ↦ binary (dependsNonZero ((\x y -> toNumber x `pow` toNumber y) `union2'` pow)),
   "/"         ↦ binary (dependsNonZero ((\x y -> toNumber x / toNumber y)  `union2'` (/))),
   "=="        ↦ binary (dependsBoth ((==) `union2'` (==) `unionDisj` (==))),
   "/="        ↦ binary (dependsBoth ((/=) `union2'` (/=) `unionDisj` (==))),
   "<"         ↦ binary (dependsBoth ((<)  `union2'` (<)  `unionDisj` (==))),
   ">"         ↦ binary (dependsBoth ((>)  `union2'` (>)  `unionDisj` (==))),
   "<="        ↦ binary (dependsBoth ((<=) `union2'` (<=) `unionDisj` (==))),
   ">="        ↦ binary (dependsBoth ((>=) `union2'` (>=) `unionDisj` (==))),
   "++"        ↦ binary (dependsBoth ((<>) :: String -> String -> String)),
   ":"         ↦ Constr false cCons Nil,
   "!"         ↦ binary (dependsNeither matrixLookup),
   "ceiling"   ↦ unary (depends ceil),
   "debugLog"  ↦ unary (depends debugLog),
   "dims"      ↦ unary (depends dims),
   "div"       ↦ binary (dependsNonZero (div :: Int -> Int -> Int)),
   "error"     ↦ unary (depends  (error :: String -> Boolean)),
   "floor"     ↦ unary (depends floor),
   "log"       ↦ unary (depends ((toNumber >>> log) `union` log)),
   "numToStr"  ↦ unary (depends (show `union` show))
]

debugLog :: Val 𝔹 -> Val 𝔹
debugLog x = trace x (const x)

dims :: MatrixRep 𝔹 -> Val 𝔹 × Val 𝔹
dims (_ × (i × α) × (j × β)) = Int α i × Int β j

dims_bwd :: Val 𝔹 × Val 𝔹 -> MatrixRep 𝔹 -> MatrixRep 𝔹
dims_bwd (Int α i' × Int β j') (vss × (i × _) × (j × _)) | i == i' && j == j' = vss × (i × α) × (j × β)
dims_bwd _ _                                                                  = error absurd

matrixLookup :: MatrixRep 𝔹 -> (Int × 𝔹) × (Int × 𝔹) -> Val 𝔹
matrixLookup (vss × _ × _) ((i × _) × (j × _)) = vss!(i - 1)!(j - 1)

matrixLookup_bwd :: Val 𝔹 -> MatrixRep 𝔹 × (Int × 𝔹) × (Int × 𝔹) -> MatrixRep 𝔹 × (Int × 𝔹) × (Int × 𝔹)
matrixLookup_bwd v ((vss × _ × _) × (i × _) × (j × _)) = (((<$>) (const Hole)) <$> vss) × ?_ × ?_ × ?_ × ?_

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
testPrim = apply (apply (binary (dependsNonZero ((*) `union2` (*)))) (Int false 0)) (Int true 0)
