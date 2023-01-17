module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Data.Array ((!!))
import Data.Exists (mkExists)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldWithIndexM, foldlWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Profunctor.Strong (first, second)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict, (\\))
import Dict (disjointUnion, empty, fromFoldable, insert, intersectionWith, lookup, singleton, unzip) as D
import Eval (apply, apply2)
import EvalBwd (apply2Bwd, applyBwd)
import Lattice (Raw, (∨), (∧), bot, botOf, erase)
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr, val)
import Trace (AppTrace)
import Util (type (+), type (×), Endo, MayFail, error, orElse, report, (×))
import Val (Array2, Env, ForeignOp, ForeignOp'(..), Fun(..), OpBwd, OpFwd, Val(..), updateMatrix)

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
   , "dict_intersectionWith" × extern dict_intersectionWith
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
   fwd (Dictionary α1 βvs1 : Dictionary α2 βvs2 : Nil) =
      pure $ unit × Dictionary (α1 ∧ α2) (βvs1 \\ βvs2)
   fwd _ = report "Dictionaries expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α βvs) =
      Dictionary α βvs : Dictionary α D.empty : Nil

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
   fwd (Dictionary α1 βvs1 : Dictionary α2 βvs2 : Nil) =
      pure $ ((const unit <$> βvs1) × (const unit <$> βvs2)) × Dictionary (α1 ∧ α2) (D.disjointUnion βvs1 βvs2)
   fwd _ = report "Dictionaries expected"

   bwd :: Partial => OpBwd (Dict Unit × Dict Unit)
   bwd ((βvs1 × βvs2) × Dictionary α βvs) =
      Dictionary α (βvs \\ βvs2) : Dictionary α (βvs \\ βvs1) : Nil

dict_foldl :: ForeignOp
dict_foldl = mkExists $ ForeignOp' { arity: 3, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd (Raw Val × Dict (AppTrace × AppTrace))
   fwd (v : u : Dictionary _ βvs : Nil) = do
      tss × u' <-
         foldWithIndexM
            (\k (tss × u1) (_ × u2) -> apply2 (v × u1 × u2) <#> first (flip (D.insert k) tss))
            (D.empty × u)
            βvs
            :: MayFail (Dict (AppTrace × AppTrace) × Val _)
      pure $ (erase v × tss) × u'
   fwd _ = report "Function, value and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × Dict (AppTrace × AppTrace))
   bwd ((v × tss) × u) = v' : u' : Dictionary bot βvs : Nil
      where
      v' × u' × βvs = foldlWithIndex
         ( \k (v1 × u' × βvs) ts ->
              let v2 × u1 × u2 = apply2Bwd (ts × u') in (v1 ∨ v2) × u1 × D.insert k (bot × u2) βvs
         )
         (botOf v × u × D.empty)
         tss

dict_get :: ForeignOp
dict_get = mkExists $ ForeignOp' { arity: 2, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd String
   fwd (Str _ k : Dictionary _ αvs : Nil) =
      (k × _) <$> (snd <$> D.lookup k αvs # orElse ("Key \"" <> k <> "\" not found"))
   fwd _ = report "String and dictionary expected"

   bwd :: Partial => OpBwd String
   bwd (k × v) =
      Str bot k : Dictionary bot (D.singleton k (bot × v)) : Nil

dict_intersectionWith :: ForeignOp
dict_intersectionWith = mkExists $ ForeignOp' { arity: 3, op: fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: OpFwd (Raw Val × Dict (AppTrace × AppTrace))
   fwd (v : Dictionary α1 βus : Dictionary α2 βus' : Nil) = do
      βttvs <-
         sequence $
            D.intersectionWith (\(β × u) (β' × u') -> (β ∧ β' × _) <$> apply2 (v × u × u')) βus βus'
            :: MayFail (Dict (_ × (AppTrace × AppTrace) × Val _))
      pure $ (erase v × (βttvs <#> snd >>> fst)) × Dictionary (α1 ∧ α2) (βttvs <#> second snd)
   fwd _ = report "Function and two dictionaries expected"

   bwd :: Partial => OpBwd (Raw Val × Dict (AppTrace × AppTrace))
   bwd ((v × tts) × Dictionary α βvs) =
      ( foldl (∨) (botOf v) (βvuus <#> (snd >>> fst))
           : Dictionary α (βvuus <#> (second (snd >>> fst)))
           : Dictionary α (βvuus <#> (second (snd >>> snd)))
           : Nil
      )
      where
      βvuus =
         D.intersectionWith (\ts (β × v') -> β × apply2Bwd (ts × v')) tts βvs
            :: Dict (_ × Val _ × Val _ × Val _)

dict_map :: ForeignOp
dict_map = mkExists $ ForeignOp' { arity: 2, op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   fwd :: Partial => OpFwd (Raw Val × Dict AppTrace)
   fwd (v : Dictionary α βvs : Nil) = do
      ts × βus <- D.unzip <$> traverse (\(β × u) -> second (β × _) <$> apply (v × u)) βvs
      pure $ (erase v × ts) × Dictionary α βus

   bwd :: Partial => OpBwd (Raw Val × Dict AppTrace)
   bwd ((v × ts) × Dictionary α βus) =
      (foldl (∨) (botOf v) us) : Dictionary α βvs : Nil
      where
      us × βvs = D.unzip $ D.intersectionWith (\t (β × u) -> second (β × _) $ applyBwd (t × u)) ts βus

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
equals = (==) `union` ((==) `unionStr` (==))

-- (Int + Number) + String -> (Int + Number) + String -> Boolean

notEquals' :: Number + String -> Number + String -> Boolean
notEquals' = (/=) `unionStr` (/=)

notEquals :: Int + Number + String -> Int + Number + String -> Boolean
notEquals = (/=) `union` ((/=) `unionStr` (/=))

lessThan :: Int + Number + String -> Int + Number + String -> Boolean
lessThan = (<) `union` ((<) `unionStr` (<))

greaterThan :: Int + Number + String -> Int + Number + String -> Boolean
greaterThan = (>) `union` ((>) `unionStr` (>))

lessThanEquals :: Int + Number + String -> Int + Number + String -> Boolean
lessThanEquals = (<=) `union` ((<=) `unionStr` (<=))

greaterThanEquals :: Int + Number + String -> Int + Number + String -> Boolean
greaterThanEquals = (>=) `union` ((>=) `unionStr` (>=))

concat :: String -> Endo String
concat = (<>)

numToStr :: Int + Number -> String
numToStr = show `union1` show

log :: Int + Number -> Number
log = (toNumber >>> N.log) `union1` N.log
