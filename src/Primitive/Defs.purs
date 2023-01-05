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
import Lattice (Raw, (∧), bot, botOf, top)
import Prelude (div, mod) as P
import Primitive
   ( BinarySlicer
   , Unary
   , binary
   , binary_
   , binaryZero
   , boolean
   , dict
   , function
   , int
   , intOrNumber
   , intOrNumberOrString
   , intPair
   , matrixRep
   , number
   , string
   , unary
   , union
   , union1
   , unionStr
   , val
   , withInverse1
   , withInverse2
   )
import Util (Endo, type (×), (×), type (+), (!), error, successful)
import Val (class Ann, Env, Fun(..), MatrixRep, Val(..), updateMatrix)

primitives :: Raw Env
primitives = D.fromFoldable
   [ ":" × Fun (PartialConstr bot cCons Nil)
   , "ceiling" × unary (number × int × withInverse1 ceil)
   , "debugLog" × unary (val × val × withInverse1 debugLog)
   , "dims" × unary (matrixRep × intPair × dims)
   , "error" × unary (string × val × withInverse1 error_)
   , "floor" × unary (number × int × withInverse1 floor)
   , "log" × unary (intOrNumber × number × withInverse1 log)
   , "numToStr" × unary (intOrNumber × string × withInverse1 numToStr)
   , "+" × binary (intOrNumber × intOrNumber × intOrNumber × withInverse2 plus)
   , "-" × binary (intOrNumber × intOrNumber × intOrNumber × withInverse2 minus)
   , "*" × binaryZero (intOrNumber × intOrNumber × withInverse2 times)
   , "**" × binaryZero (intOrNumber × intOrNumber × withInverse2 pow)
   , "/" × binaryZero (intOrNumber × intOrNumber × withInverse2 divide)
   , "==" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 equals)
   , "/=" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 notEquals)
   , "<" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 lessThan)
   , ">" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 greaterThan)
   , "<=" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 lessThanEquals)
   , ">=" × binary (intOrNumberOrString × intOrNumberOrString × boolean × withInverse2 greaterThanEquals)
   , "++" × binary (string × string × string × withInverse2 concat)
   , "!" × binary_ matrixLookup
   , "div" × binaryZero (int × int × withInverse2 div)
   , "get" × binary_ get
   , "mod" × binaryZero (int × int × withInverse2 mod)
   , "quot" × binaryZero (int × int × withInverse2 quot)
   , "rem" × binaryZero (int × int × withInverse2 rem)
   ]

debugLog :: forall a. Val a -> Val a
debugLog x = trace x (const x)

error_ :: forall a. String -> Val a
error_ = error

dims :: forall a. Unary (MatrixRep a) ((Int × a) × (Int × a))
dims = { fwd, bwd }
   where
   fwd :: MatrixRep a -> (Int × a) × (Int × a)
   fwd (_ × iα × jβ) = iα × jβ

   bwd :: (Int × a) × (Int × a) -> Endo (MatrixRep a)
   bwd (iα × jβ) (vss × _ × _) = vss × iα × jβ

lookup_fwd :: forall a. Partial => List (Val a) -> Val a
lookup_fwd (Matrix _ (vss × _ × _) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
   | c == cPair = vss ! (i - 1) ! (j - 1)

lookup_bwd :: forall a. Partial => Ann a => Val a -> List (Raw Val) -> List (Val a)
lookup_bwd v (Matrix _ (vss × (i' × _) × (j' × _)) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
   | c == cPair =
        Matrix bot (updateMatrix i j (const v) ((((<$>) botOf) <$> vss) × (i' × bot) × (j' × bot)))
           : Constr bot cPair (Int bot i : Int bot j : Nil)
           : Nil

-- Unfortunately the primitives infrastructure doesn't generalise to "deep" pattern-matching/construction. Here
-- non-neededness of matrix bounds/indices should arise automtically because construction rights are not required.
matrixLookup :: forall a. BinarySlicer (MatrixRep a) ((Int × a) × (Int × a)) (Val a) a
matrixLookup = { i1: matrixRep, i2: intPair, o: val, fwd: fwd', bwd: bwd' }
   where
   fwd :: MatrixRep a -> (Int × a) × (Int × a) -> Val a
   fwd (vss × _ × _) ((i × _) × (j × _)) = vss ! (i - 1) ! (j - 1)

   bwd :: Ann a => Val a -> MatrixRep a × ((Int × a) × (Int × a)) -> MatrixRep a × ((Int × a) × (Int × a))
   bwd v (vss × (i' × _) × (j' × _) × ((i × _) × (j × _))) =
      updateMatrix i j (const v) (vss × (i' × bot) × (j' × bot)) × ((i × bot) × (j × bot))

   fwd' :: Ann a => MatrixRep a × a -> ((Int × a) × (Int × a)) × a -> Val a × a
   fwd' (x × α) (y × β) = fwd x y × (α ∧ β)

   bwd' :: Ann a => Val a × a -> MatrixRep a × ((Int × a) × (Int × a)) -> (MatrixRep a × a) × ((Int × a) × (Int × a) × a)
   bwd' (z × α) (x × y) = (x' × α) × (y' × α)
      where
      x' × y' = bwd z (x × y)

get :: forall a. BinarySlicer String (Dict (a × Val a)) (Val a) a
get = { i1: string, i2: dict, o: val, fwd, bwd }
   where
   fwd :: Ann a => String × a -> Dict (a × Val a) × a -> Val a × a
   fwd (k × _) (d × _) = snd (D.get k d) × top

   bwd :: Ann a => Val a × a -> String × Dict (a × Val a) -> (String × a) × (Dict (a × Val a) × a)
   bwd (v × _) (k × _) = (k × bot) × (D.singleton k (bot × v) × bot)

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
