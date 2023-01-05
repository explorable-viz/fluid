module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Tuple (snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict)
import Dict (fromFoldable, get, singleton) as D
import Eval (apply)
import Lattice (Raw, bot, botOf)
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (BinarySlicer, binary, binaryZero, boolean, dict, function, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr, val)
import Util (Endo, type (×), (×), type (+), (!), error, successful)
import Val (class Ann, Env, Fun(..), OpBwd, OpFwd, PrimOp(..), Val(..), updateMatrix)

primitives :: Raw Env
primitives = D.fromFoldable
   [ ":" × Fun (PartialConstr bot cCons Nil)
   , "ceiling" × unary { i: number, o: int, fwd: ceil }
   , "debugLog" × unary { i: val, o: val, fwd: debugLog }
   , "dims" × Fun (Primitive dims Nil)
   , "error" × unary { i: string, o: val, fwd: error_ }
   , "floor" × unary { i: number, o: int, fwd: floor }
   , "log" × unary { i: intOrNumber, o: number, fwd: log }
   , "numToStr" × unary { i: intOrNumber, o: string, fwd: numToStr }
   , "+" × binary intOrNumber intOrNumber intOrNumber plus
   , "-" × binary intOrNumber intOrNumber intOrNumber minus
   , "*" × binaryZero intOrNumber intOrNumber times
   , "**" × binaryZero intOrNumber intOrNumber pow
   , "/" × binaryZero intOrNumber intOrNumber divide
   , "==" × binary intOrNumberOrString intOrNumberOrString boolean equals
   , "/=" × binary intOrNumberOrString intOrNumberOrString boolean notEquals
   , "<" × binary intOrNumberOrString intOrNumberOrString boolean lessThan
   , ">" × binary intOrNumberOrString intOrNumberOrString boolean greaterThan
   , "<=" × binary intOrNumberOrString intOrNumberOrString boolean lessThanEquals
   , ">=" × binary intOrNumberOrString intOrNumberOrString boolean greaterThanEquals
   , "++" × binary string string string concat
   , "!" × Fun (Primitive matrixLookup Nil)
   , "div" × binaryZero int int div
   , "get" × Fun (Primitive get Nil)
   , "mod" × binaryZero int int mod
   , "quot" × binaryZero int int quot
   , "rem" × binaryZero int int rem
   ]

debugLog :: forall a. Val a -> Val a
debugLog x = trace x (const x)

error_ :: forall a. String -> Val a
error_ = error

dims :: PrimOp
dims = PrimOp { arity: 1, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Matrix α (_ × (i × β1) × (j × β2)) : Nil) = Constr α cPair (Int β1 i : Int β2 j : Nil)

   bwd :: Partial => OpBwd
   bwd (Constr α c (Int β1 i : Int β2 j : Nil)) (Matrix _ (vss × _ × _) : Nil) | c == cPair =
      Matrix α (((<$>) botOf <$> vss) × (i × β1) × (j × β2)) : Nil

matrixLookup :: PrimOp
matrixLookup = PrimOp { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Matrix _ (vss × _ × _) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = vss ! (i - 1) ! (j - 1)

   bwd :: Partial => OpBwd
   bwd v (Matrix _ (vss × (i' × _) × (j' × _)) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair =
           Matrix bot (updateMatrix i j (const v) (((<$>) botOf <$> vss) × (i' × bot) × (j' × bot)))
              : Constr bot cPair (Int bot i : Int bot j : Nil)
              : Nil

get :: PrimOp
get = PrimOp { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Str _ k : Dictionary _ d : Nil) = snd (D.get k d)

   bwd :: Partial => OpBwd
   bwd v (Str _ k : Dictionary _ _ : Nil) = (Str bot k) : Dictionary bot (D.singleton k (bot × v)) : Nil

map :: forall a. BinarySlicer (Fun a) (Dict (a × Val a)) (Dict (a × Val a)) a
map = { i1: function, i2: dict, o: dict, fwd, bwd }
   where
   fwd :: Ann a => Fun a × a -> Dict (a × Val a) × a -> Dict (a × Val a) × a
   fwd (φ × _) (d × α) = (d <#> (\(β × v) -> β × snd (successful (apply φ v)))) × α

   bwd :: Ann a => _
   bwd = error "todo"

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
divide = (\x y -> toNumber x / toNumber y) `union` (/)

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
lessThan = (<) `union` (<) `unionStr` (<)

greaterThan :: Int + Number + String -> Int + Number + String -> Boolean
greaterThan = (>) `union` (>) `unionStr` (>)

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
