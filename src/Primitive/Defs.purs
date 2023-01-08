module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Data.Array ((!!))
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Traversable (traverse)
import Data.Tuple (snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict)
import Dict (fromFoldable, intersectionWith, lookup, singleton) as D
import Eval (apply)
import EvalBwd (applyBwd)
import Lattice (Raw, bot, botOf)
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr, val)
import Util (Endo, type (×), (×), type (+), error, orElse)
import Val (Env, Fun(..), OpBwd, OpFwd, PrimOp(..), Val(..), updateMatrix)

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
   , "+" × binary { i1: intOrNumber, i2: intOrNumber, o: intOrNumber, fwd: plus }
   , "-" × binary { i1: intOrNumber, i2: intOrNumber, o: intOrNumber, fwd: minus }
   , "*" × binaryZero { i: intOrNumber, o: intOrNumber, fwd: times }
   , "**" × binaryZero { i: intOrNumber, o: intOrNumber, fwd: pow }
   , "/" × binaryZero { i: intOrNumber, o: intOrNumber, fwd: divide }
   , "==" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: equals }
   , "/=" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: notEquals }
   , "<" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: lessThan }
   , ">" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: greaterThan }
   , "<=" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: lessThanEquals }
   , ">=" × binary { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: greaterThanEquals }
   , "++" × binary { i1: string, i2: string, o: string, fwd: concat }
   , "!" × Fun (Primitive matrixLookup Nil)
   , "div" × binaryZero { i: int, o: int, fwd: div }
   , "get" × Fun (Primitive get Nil)
   , "mod" × binaryZero { i: int, o: int, fwd: mod }
   , "quot" × binaryZero { i: int, o: int, fwd: quot }
   , "rem" × binaryZero { i: int, o: int, fwd: rem }
   ]

debugLog :: forall a. Val a -> Val a
debugLog x = trace x (const x)

error_ :: forall a. String -> Val a
error_ = error

dims :: PrimOp
dims = PrimOp { arity: 1, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Matrix α (_ × (i × β1) × (j × β2)) : Nil) =
      pure $ Constr α cPair (Int β1 i : Int β2 j : Nil)

   bwd :: Partial => OpBwd
   bwd (Constr α c (Int β1 i : Int β2 j : Nil)) (Matrix _ (vss × _ × _) : Nil) | c == cPair =
      Matrix α (((<$>) botOf <$> vss) × (i × β1) × (j × β2)) : Nil

matrixLookup :: PrimOp
matrixLookup = PrimOp { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Matrix _ (vss × _ × _) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = orElse "Index out of bounds" $ do
           vs <- vss !! (i - 1)
           vs !! (j - 1)

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
   fwd (Str _ k : Dictionary _ d : Nil) =
      snd <$> (D.lookup k d # orElse ("Key \"" <> k <> "\" not found"))

   bwd :: Partial => OpBwd
   bwd v (Str _ k : Dictionary _ _ : Nil) =
      (Str bot k) : Dictionary bot (D.singleton k (bot × v)) : Nil

map :: PrimOp
map = PrimOp { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd
   fwd (Fun φ : Dictionary α d : Nil) =
      Dictionary α <$> traverse (\(β × v) -> (snd >>> (β × _)) <$> apply φ v) d

   bwd :: Partial => OpBwd
   bwd (Dictionary α d') (Fun φ : Dictionary _ d : Nil) =
      let d'' = D.intersectionWith (\(_ × u) (β × v) -> β × applyBwd v ?_) d d'
                :: Dict (_ × (Fun _ × Val _)) in
      Fun (foldl ?_ (botOf φ) d'') : Dictionary ?_ ?_ : Nil

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
