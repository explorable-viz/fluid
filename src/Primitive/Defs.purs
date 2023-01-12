module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Data.Array ((!!))
import Data.Exists (mkExists)
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Profunctor.Strong (second)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict, (\\))
import Dict (disjointUnion, empty, fromFoldable, intersectionWith, lookup, singleton, unzip) as D
import Eval (apply)
import EvalBwd (applyBwd)
import Lattice (Raw, (∨), (∧), bot, botOf, erase)
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr, val)
import Trace (AppTrace)
import Util (type (+), type (×), Endo, error, orElse, report, (×))
import Val (Array2, Env, ForeignOp'(..), Fun(..), OpBwd, OpFwd, Val(..), ForeignOp, updateMatrix)

extern :: forall a. ForeignOp -> Val a
extern = flip Foreign Nil >>> Fun

primitives :: Raw Env
primitives = D.fromFoldable
   [ ":" × Fun (PartialConstr bot cCons Nil)
   , "ceiling" × unary { i: number, o: int, fwd: ceil }
   , "debugLog" × unary { i: val, o: val, fwd: debugLog }
   , "dims" × extern dims
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
   , "!" × extern matrixLookup
   , "dict_difference" × extern dict_difference
   , "dict_disjointUnion" × extern dict_disjointUnion
   , "dict_fromRecord" × extern dict_fromRecord
   , "dict_get" × extern dict_get
   , "dict_map" × extern dict_map
   , "div" × binaryZero { i: int, o: int, fwd: div }
   , "mod" × binaryZero { i: int, o: int, fwd: mod }
   , "quot" × binaryZero { i: int, o: int, fwd: quot }
   , "rem" × binaryZero { i: int, o: int, fwd: rem }
   ]

debugLog :: forall a. Val a -> Val a
debugLog x = trace x (const x)

error_ :: forall a. String -> Val a
error_ = error

type ArrayData a = Array2 (Val a)

dims :: ForeignOp
dims = mkExists $ ForeignOp' { arity: 1, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd (Raw ArrayData)
   fwd (Matrix α (vss × (i × β1) × (j × β2)) : Nil) =
      pure $ (map erase <$> vss) × Constr α cPair (Int β1 i : Int β2 j : Nil)
   fwd _ = report "Matrix expected"

   bwd :: Partial => OpBwd (Raw ArrayData)
   bwd (vss × Constr α c (Int β1 i : Int β2 j : Nil)) | c == cPair =
      Matrix α (((<$>) botOf <$> vss) × (i × β1) × (j × β2)) : Nil

matrixLookup :: ForeignOp
matrixLookup = mkExists $ ForeignOp' { arity: 2, op: fwd, op_bwd: bwd }
   where
   fwd :: OpFwd (Raw ArrayData × (Int × Int) × (Int × Int))
   fwd (Matrix _ (vss × (i' × _) × (j' × _)) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = do
           v <- orElse "Index out of bounds" $ do
              us <- vss !! (i - 1)
              us !! (j - 1)
           pure $ ((map erase <$> vss) × (i' × j') × (i × j)) × v
   fwd _ = report "Matrix and pair of integers expected"

   bwd :: OpBwd (Raw ArrayData × (Int × Int) × (Int × Int))
   bwd ((vss × (i' × j') × (i × j)) × v) =
      Matrix bot (updateMatrix i j (const v) (((<$>) botOf <$> vss) × (i' × bot) × (j' × bot)))
         : Constr bot cPair (Int bot i : Int bot j : Nil)
         : Nil

dict_difference :: ForeignOp
dict_difference = mkExists $ ForeignOp' { arity: 2, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd Unit
   fwd (Dictionary α1 d1 : Dictionary α2 d2 : Nil) =
      pure $ unit × Dictionary (α1 ∧ α2) (d1 \\ d2)
   fwd _ = report "Dictionaries expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α d) =
      Dictionary α d : Dictionary α D.empty : Nil

dict_fromRecord :: ForeignOp
dict_fromRecord = mkExists $ ForeignOp' { arity: 1, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd Unit
   fwd (Record α xvs : Nil) =
      pure $ unit × Dictionary α (xvs <#> (α × _))
   fwd _ = report "Record expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α d) = Record (foldl (∨) α (d <#> fst)) (d <#> snd) : Nil

dict_disjointUnion :: ForeignOp
dict_disjointUnion = mkExists $ ForeignOp' { arity: 2, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd (Dict Unit × Dict Unit)
   fwd (Dictionary α1 d1 : Dictionary α2 d2 : Nil) = 
      pure $ ((const unit <$> d1) × (const unit <$> d2)) × Dictionary (α1 ∧ α2) (D.disjointUnion d1 d2)
   fwd _ = report "Dictionaries expected"

   bwd :: Partial => OpBwd (Dict Unit × Dict Unit)
   bwd ((d1 × d2) × Dictionary α d) = 
      Dictionary α (d \\ d2) : Dictionary α (d \\ d1) : Nil

dict_get :: ForeignOp
dict_get = mkExists $ ForeignOp' { arity: 2, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd String
   fwd (Str _ k : Dictionary _ d : Nil) =
      (k × _) <$> (snd <$> D.lookup k d # orElse ("Key \"" <> k <> "\" not found"))
   fwd _ = report "String and dictionary expected"

   bwd :: Partial => OpBwd String
   bwd (k × v) =
      Str bot k : Dictionary bot (D.singleton k (bot × v)) : Nil

dict_map :: ForeignOp
dict_map = mkExists $ ForeignOp' { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd (Raw Fun × Dict AppTrace)
   fwd (Fun φ : Dictionary α βvs : Nil) = do
      ts × βus <- D.unzip <$> traverse (\(β × v) -> second (β × _) <$> apply φ v) βvs
      pure $ erase φ × ts × Dictionary α βus

   bwd :: Partial => OpBwd (Raw Fun × Dict AppTrace)
   bwd (φ × ts × Dictionary α βus) =
      Fun (foldl (∨) (botOf φ) φs) : Dictionary α βvs : Nil
      where
      φs × βvs = D.unzip $ D.intersectionWith (\t (β × u) -> second (β × _) $ applyBwd u t) ts βus

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
