module Primitive.Defs where

import Prelude hiding (absurd, div)
import Prelude (div) as P
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..))
import Data.Map (Map, fromFoldable)
import Debug.Trace (trace)
import Math (log, pow) as M
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), (:+:), (↦))
import DataType (cCons)
import Lattice (𝔹)
import Primitive (
   Binary, BinarySpec, OpDef, UnarySpec,
   binary, depends, depends2, dependsBoth, dependsBoth2, dependsZero, opDef, unary, union, union1, unionStr
)
import Util (type (×), (×), type (+), (≜), (!), absurd, error, unsafeUpdateAt)
import Val (Env, MatrixRep, Val(..))

-- Syntactic information only. No requirement that any of these be defined.
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

primitives :: Env 𝔹
primitives = foldl (:+:) Empty [
   ":"         ↦ Constr false cCons Nil,
   "+"         ↦ binary (dependsBoth2 plus),
   "-"         ↦ binary (dependsBoth2 minus),
   "*"         ↦ binary (dependsZero times),
   "**"        ↦ binary (dependsZero pow),
   "/"         ↦ binary (dependsZero divide),
   "=="        ↦ binary (dependsBoth equals),
   "/="        ↦ binary (dependsBoth notEquals),
   "<"         ↦ binary (dependsBoth lessThan),
   ">"         ↦ binary (dependsBoth greaterThan),
   "<="        ↦ binary (dependsBoth lessThanEquals),
   ">="        ↦ binary (dependsBoth greaterThanEquals),
   "++"        ↦ binary (dependsBoth concat),
   "!"         ↦ binary matrixLookup,
   "ceiling"   ↦ unary (depends ceil),
   "debugLog"  ↦ unary (depends debugLog),
   "dims"      ↦ unary dims,
   "div"       ↦ binary (dependsZero div),
   "error"     ↦ unary (depends error_),
   "floor"     ↦ unary (depends floor),
   "log"       ↦ unary (depends log),
   "numToStr"  ↦ unary (depends numToStr)
]

debugLog :: Val 𝔹 -> Val 𝔹
debugLog x = trace x (const x)

error_ :: String -> Val 𝔹
error_ = error

dims :: UnarySpec (MatrixRep 𝔹) (Val 𝔹 × Val 𝔹)
dims = depends2 { f, g }
   where
   f :: MatrixRep 𝔹 -> Val 𝔹 × Val 𝔹
   f (_ × (i × β) × (j × β')) = Int β i × Int β' j

   g :: Val 𝔹 × Val 𝔹 -> MatrixRep 𝔹 -> MatrixRep 𝔹
   g (Int β i' × Int β' j') (vss × (i × _) × (j × _))  = vss × ((i ≜ i') × β) × ((j ≜ j') × β')
   g (_ × _) _                                         = error absurd

matrixLookup :: BinarySpec (MatrixRep 𝔹) ((Int × 𝔹) × (Int × 𝔹)) (Val 𝔹)
matrixLookup = dependsBoth2 { f, g }
   where
   f :: MatrixRep 𝔹 -> (Int × 𝔹) × (Int × 𝔹) -> Val 𝔹
   f (vss × _ × _) ((i × _) × (j × _)) = vss!(i - 1)!(j - 1)

   g :: Val 𝔹 -> MatrixRep 𝔹 × ((Int × 𝔹) × (Int × 𝔹)) -> MatrixRep 𝔹 × ((Int × 𝔹) × (Int × 𝔹))
   g v (vss × (i' × _) × (j' × _) × ((i × _) × (j × _))) =
     (vss'' × (i' × false) × (j' × false)) × ((i × false) × (j × false))
     where vss'  = (<$>) (const Hole) <$> vss
           vs_i  = vss'!(i - 1)
           vss'' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) v vs_i) vss'

plus :: Binary (Int + Number) (Int + Number) (Int + Number)
plus = { f: (+) `union` (+), g: const identity }

minus :: Binary (Int + Number) (Int + Number) (Int + Number)
minus = { f: (-) `union` (-), g: const identity }

times :: Binary (Int + Number) (Int + Number) (Int + Number)
times = { f: (*) `union` (*), g: const identity }

-- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
pow :: Binary (Int + Number) (Int + Number) (Int + Number)
pow = { f: (\x y -> toNumber x `M.pow` toNumber y) `union` M.pow, g: const identity }

divide :: Binary (Int + Number) (Int + Number) (Int + Number)
divide = { f: (\x y -> toNumber x / toNumber y)  `union` (/), g: const identity }

div :: Binary Int Int Int
div = { f: P.div, g: const identity }

equals :: Int + Number + String -> Int + Number + String -> Boolean
equals = (==) `union` (==) `unionStr` (==)

notEquals :: Int + Number + String -> Int + Number + String -> Boolean
notEquals = (/=) `union` (/=) `unionStr` (/=)

lessThan :: Int + Number + String -> Int + Number + String -> Boolean
lessThan = (<)  `union` (<)  `unionStr` (<)

greaterThan :: Int + Number + String -> Int + Number + String -> Boolean
greaterThan = (>)  `union` (>)  `unionStr` (>)

lessThanEquals :: Int + Number + String -> Int + Number + String -> Boolean
lessThanEquals = (<=) `union` (<=) `unionStr` (<=)

greaterThanEquals :: Int + Number + String -> Int + Number + String -> Boolean
greaterThanEquals = (>=) `union` (>=) `unionStr` (>=)

concat :: String -> String -> String
concat = (<>)

numToStr :: Int + Number -> String
numToStr = show `union1` show

log :: Int + Number -> Number
log = (toNumber >>> M.log) `union1` M.log
