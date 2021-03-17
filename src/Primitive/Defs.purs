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

   "ceiling"   ↦ unary (depends1 (withInverse1 ceil)),
   "debugLog"  ↦ unary (depends1 (withInverse1 debugLog)),
   "dims"      ↦ unary dims,
   "error"     ↦ unary (depends1 (withInverse1 error_)),
   "floor"     ↦ unary (depends1 (withInverse1 floor)),
   "log"       ↦ unary (depends1 (withInverse1 log)),
   "numToStr"  ↦ unary (depends1 (withInverse1 numToStr))
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
plus = withInverse2 ((+) `union` (+))

minus :: Binary (Int + Number) (Int + Number) (Int + Number)
minus = withInverse2 ((-) `union` (-))

times :: Binary (Int + Number) (Int + Number) (Int + Number)
times = withInverse2 ((*) `union` (*))

-- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
pow :: Binary (Int + Number) (Int + Number) (Int + Number)
pow = withInverse2 ((\x y -> toNumber x `M.pow` toNumber y) `union` M.pow)

divide :: Binary (Int + Number) (Int + Number) (Int + Number)
divide = withInverse2 ((\x y -> toNumber x / toNumber y)  `union` (/))

div :: Binary Int Int Int
div = withInverse2 P.div

equals :: Binary (Int + Number + String) (Int + Number + String) Boolean
equals = withInverse2 ((==) `union` (==) `unionStr` (==))

notEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
notEquals = withInverse2 ((/=) `union` (/=) `unionStr` (/=))

lessThan :: Binary (Int + Number + String) (Int + Number + String) Boolean
lessThan = withInverse2 ((<)  `union` (<)  `unionStr` (<))

greaterThan :: Binary (Int + Number + String) (Int + Number + String) Boolean
greaterThan = withInverse2 ((>)  `union` (>)  `unionStr` (>))

lessThanEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
lessThanEquals = withInverse2 ((<=) `union` (<=) `unionStr` (<=))

greaterThanEquals :: Binary (Int + Number + String) (Int + Number + String) Boolean
greaterThanEquals = withInverse2 ((>=) `union` (>=) `unionStr` (>=))

concat :: Binary String String String
concat = withInverse2 (<>)

numToStr :: Int + Number -> String
numToStr = show `union1` show

log :: Int + Number -> Number
log = (toNumber >>> M.log) `union1` M.log
