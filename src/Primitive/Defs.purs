module Primitive.Defs where

import Prelude hiding (absurd, div, mod)
import Prelude (div, mod) as P
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..))
import Data.Number (log, pow) as N
import Debug (trace)
import DataType (cCons)
import Dict (fromFoldable) as D
import Lattice (𝔹)
import Primitive (Binary, Unary, binary, binaryZero, unary, union, union1, unionStr, withInverse1, withInverse2)
import Util (Endo, type (×), (×), type (+), (!), error)
import Val (Env, MatrixRep, Val(..), updateMatrix)

primitives :: Env 𝔹
primitives = D.fromFoldable [
   ":"         × Constr false cCons Nil,

   "ceiling"   × unary (withInverse1 ceil),
   "debugLog"  × unary (withInverse1 debugLog),
   "dims"      × unary dims,
   "error"     × unary (withInverse1 error_),
   "floor"     × unary (withInverse1 floor),
   "log"       × unary (withInverse1 log),
   "numToStr"  × unary (withInverse1 numToStr),

   "+"         × binary (withInverse2 plus),
   "-"         × binary (withInverse2 minus),
   "*"         × binaryZero (withInverse2 times),
   "**"        × binaryZero (withInverse2 pow),
   "/"         × binaryZero (withInverse2 divide),
   "=="        × binary (withInverse2 equals),
   "/="        × binary (withInverse2 notEquals),
   "<"         × binary (withInverse2 lessThan),
   ">"         × binary (withInverse2 greaterThan),
   "<="        × binary (withInverse2 lessThanEquals),
   ">="        × binary (withInverse2 greaterThanEquals),
   "++"        × binary (withInverse2 concat),
   "!"         × binary matrixLookup,
   "div"       × binaryZero (withInverse2 div),
   "mod"       × binaryZero (withInverse2 mod),
   "quot"      × binaryZero (withInverse2 quot),
   "rem"       × binaryZero (withInverse2 rem)
]

debugLog :: Val 𝔹 -> Val 𝔹
debugLog x = trace x (const x)

error_ :: String -> Val 𝔹
error_ = error

dims :: Unary (MatrixRep 𝔹) ((Int × 𝔹) × (Int × 𝔹))
dims = { fwd, bwd }
   where
   fwd :: MatrixRep 𝔹 -> (Int × 𝔹) × (Int × 𝔹)
   fwd (_ × i × j) = i × j

   bwd :: (Int × 𝔹) × (Int × 𝔹) -> Endo (MatrixRep 𝔹)
   bwd (i × j) (vss × _ × _) = vss × i × j

-- Unfortunately the primitives infrastructure doesn't generalise to "deep" pattern-matching/construction. Here
-- non-neededness of matrix bounds/indices should arise automtically because construction rights are not required.
matrixLookup :: Binary (MatrixRep 𝔹) ((Int × 𝔹) × (Int × 𝔹)) (Val 𝔹)
matrixLookup = { fwd, bwd }
   where
   fwd :: MatrixRep 𝔹 -> (Int × 𝔹) × (Int × 𝔹) -> Val 𝔹
   fwd (vss × _ × _) ((i × _) × (j × _)) = vss!(i - 1)!(j - 1)

   bwd :: Val 𝔹 -> MatrixRep 𝔹 × ((Int × 𝔹) × (Int × 𝔹)) -> MatrixRep 𝔹 × ((Int × 𝔹) × (Int × 𝔹))
   bwd v (vss × (i' × _) × (j' × _) × ((i × _) × (j × _))) =
       updateMatrix i j (const v) (vss × (i' × false) × (j' × false)) × ((i × false) × (j × false))

plus :: Int + Number -> Endo (Int + Number)
plus = (+) `union` (+)

minus :: Int + Number -> Endo (Int + Number)
minus = (-) `union` (-)

times :: Int + Number -> Endo (Int + Number)
times = (*) `union` (*)

-- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
pow :: Int + Number -> Endo (Int + Number)
pow = (\x y -> toNumber x `N.pow` toNumber y) `union` N.pow

divide :: Int + Number -> Endo (Int + Number)
divide = (\x y -> toNumber x / toNumber y)  `union` (/)

-- See T-, F- and E-definitions discussed at https://github.com/purescript/purescript-prelude/issues/161
div :: Int -> Endo Int
div = P.div

mod :: Int -> Endo Int
mod = P.mod

quot :: Int -> Endo Int
quot = I.quot

rem :: Int -> Endo Int
rem = I.rem

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

concat :: String -> Endo String
concat = (<>)

numToStr :: Int + Number -> String
numToStr = show `union1` show

log :: Int + Number -> Number
log = (toNumber >>> N.log) `union1` N.log
