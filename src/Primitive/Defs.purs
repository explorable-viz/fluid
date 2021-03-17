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
   binary, depends1, depends2, depends2Zero, opDef, unary, union, union1, unionStr, withInverse1, withInverse2
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
   "+"         ↦ binary (depends2 plus),
   "-"         ↦ binary (depends2 minus),
   "*"         ↦ binary (depends2Zero times),
   "**"        ↦ binary (depends2Zero pow),
   "/"         ↦ binary (depends2Zero divide),
   "=="        ↦ binary (depends2 equals),
   "/="        ↦ binary (depends2 notEquals),
   "<"         ↦ binary (depends2 lessThan),
   ">"         ↦ binary (depends2 greaterThan),
   "<="        ↦ binary (depends2 lessThanEquals),
   ">="        ↦ binary (depends2 greaterThanEquals),
   "++"        ↦ binary (depends2 concat),
   "!"         ↦ binary matrixLookup,
   "div"       ↦ binary (depends2Zero div),

   "ceiling"   ↦ unary (withInverse1 ceil),
   "debugLog"  ↦ unary (withInverse1 debugLog),
   "dims"      ↦ unary dims,
   "error"     ↦ unary (withInverse1 error_),
   "floor"     ↦ unary (withInverse1 floor),
   "log"       ↦ unary (withInverse1 log),
   "numToStr"  ↦ unary (withInverse1 numToStr)
]

debugLog :: Val 𝔹 -> Val 𝔹
debugLog x = trace x (const x)

error_ :: String -> Val 𝔹
error_ = error

dims :: UnarySpec (MatrixRep 𝔹) (Val 𝔹 × Val 𝔹)
dims = depends1 { f, g }
   where
   f :: MatrixRep 𝔹 -> Val 𝔹 × Val 𝔹
   f (_ × (i × β) × (j × β')) = Int β i × Int β' j

   g :: Val 𝔹 × Val 𝔹 -> MatrixRep 𝔹 -> MatrixRep 𝔹
   g (Int β i' × Int β' j') (vss × (i × _) × (j × _))  = vss × ((i ≜ i') × β) × ((j ≜ j') × β')
   g (_ × _) _                                         = error absurd

matrixLookup :: BinarySpec (MatrixRep 𝔹) ((Int × 𝔹) × (Int × 𝔹)) (Val 𝔹)
matrixLookup = depends2 { f, g }
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

equals :: Binary (Int + Number + String) (Int + Number + String) Boolean
equals = { f: (==) `union` (==) `unionStr` (==), g: const identity }

notEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
notEquals = { f: (/=) `union` (/=) `unionStr` (/=), g: const identity }

lessThan :: Binary (Int + Number + String) (Int + Number + String) Boolean
lessThan = { f: (<)  `union` (<)  `unionStr` (<), g: const identity }

greaterThan :: Binary (Int + Number + String) (Int + Number + String) Boolean
greaterThan = { f: (>)  `union` (>)  `unionStr` (>), g: const identity }

lessThanEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
lessThanEquals = { f: (<=) `union` (<=) `unionStr` (<=), g: const identity }

greaterThanEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
greaterThanEquals = { f: (>=) `union` (>=) `unionStr` (>=), g: const identity }

concat :: Binary String String String
concat = { f: (<>), g: const identity }

numToStr :: Int + Number -> String
numToStr = show `union1` show

log :: Int + Number -> Number
log = (toNumber >>> M.log) `union1` M.log
