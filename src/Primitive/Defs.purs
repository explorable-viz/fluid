module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Bind (Bind)
import Data.Exists (mkExists)
import Data.Foldable (foldl, foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Newtype (wrap)
import Data.Number (log, pow) as N
import Data.Profunctor.Strong (first, second)
import Data.Set as Set
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict)
import Dict (fromFoldable) as D
import Eval (apply, apply2)
import EvalBwd (apply2Bwd, applyBwd)
import EvalGraph (apply) as G
import Graph.WithGraph (new)
import Lattice (class BoundedJoinSemilattice, Raw, bot, botOf, erase, (∧), (∨))
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Trace (AppTrace)
import Util (type (+), type (×), Endo, error, orElse, singleton, throw, unimplemented, (×), unzip)
import Util.Map (disjointUnion, insert, intersectionWith, lookup, maplet, (\\))
import Util.Set (empty)
import Val (Array2, BaseVal(..), DictRep(..), Env, ForeignOp(..), ForeignOp'(..), Fun(..), MatrixRep(..), OpBwd, OpFwd, OpGraph, Val(..), matrixGet, matrixPut)

extern :: forall a. BoundedJoinSemilattice a => ForeignOp -> Bind (Val a)
extern (ForeignOp (id × φ)) = id × Val bot (Fun ((Foreign (ForeignOp (id × φ))) Nil))

primitives :: Raw Env
primitives = wrap $ wrap $ D.fromFoldable
   [ ":" × Val bot (Fun (PartialConstr cCons Nil))
   , unary "ceiling" { i: number, o: int, fwd: ceil }
   , extern debugLog
   , extern dims
   , extern error_
   , unary "floor" { i: number, o: int, fwd: floor }
   , unary "log" { i: intOrNumber, o: number, fwd: log }
   , unary "numToStr" { i: intOrNumber, o: string, fwd: numToStr }
   , binary "+" { i1: intOrNumber, i2: intOrNumber, o: intOrNumber, fwd: plus }
   , binary "-" { i1: intOrNumber, i2: intOrNumber, o: intOrNumber, fwd: minus }
   , binaryZero "*" { i: intOrNumber, o: intOrNumber, fwd: times }
   , binaryZero "**" { i: intOrNumber, o: intOrNumber, fwd: pow }
   , binaryZero "/" { i: intOrNumber, o: intOrNumber, fwd: divide }
   , binary "==" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: equals }
   , binary "/=" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: notEquals }
   , binary "<" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: lessThan }
   , binary ">" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: greaterThan }
   , binary "<=" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: lessThanEquals }
   , binary ">=" { i1: intOrNumberOrString, i2: intOrNumberOrString, o: boolean, fwd: greaterThanEquals }
   , binary "++" { i1: string, i2: string, o: string, fwd: concat }
   , extern matrixLookup
   , extern dict_difference
   , extern dict_disjointUnion
   , extern dict_foldl
   , extern dict_fromRecord
   , extern dict_get
   , extern dict_intersectionWith
   , extern dict_map
   , extern matrixUpdate
   , binaryZero "div" { i: int, o: int, fwd: div }
   , binaryZero "mod" { i: int, o: int, fwd: mod }
   , binaryZero "quot" { i: int, o: int, fwd: quot }
   , binaryZero "rem" { i: int, o: int, fwd: rem }
   ]

error_ :: ForeignOp
error_ =
   ForeignOp ("error" × mkExists (ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }))
   where
   op' :: OpGraph
   op' (Val _ (Str s) : Nil) = pure $ error s
   op' _ = throw "String expected"

   fwd :: OpFwd Unit
   fwd (Val _ (Str s) : Nil) = error s
   fwd _ = throw "String expected"

   bwd :: OpBwd Unit
   bwd _ = error unimplemented

debugLog :: ForeignOp
debugLog =
   ForeignOp ("debugLog" × mkExists (ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }))
   where
   op' :: OpGraph
   op' (x : Nil) = pure $ trace x (const x)
   op' _ = throw "Single value expected"

   fwd :: OpFwd Unit
   fwd (x : Nil) = pure $ unit × trace x (const x)
   fwd _ = throw "Single value expected"

   bwd :: OpBwd Unit
   bwd _ = error unimplemented

dims :: ForeignOp
dims =
   ForeignOp ("dims" × mkExists (ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val α (Matrix (MatrixRep (_ × (i × β1) × (j × β2)))) : Nil) = do
      v1 <- Val <$> new (singleton β1) <@> Int i
      v2 <- Val <$> new (singleton β2) <@> Int j
      Val <$> new (singleton α) <@> Constr cPair (v1 : v2 : Nil)
   op _ = throw "Matrix expected"

   fwd :: OpFwd (Array2 (Raw Val))
   fwd (Val α (Matrix (MatrixRep (vss × (i × β1) × (j × β2)))) : Nil) =
      pure $ (map erase <$> vss) × Val α (Constr cPair (Val β1 (Int i) : Val β2 (Int j) : Nil))
   fwd _ = throw "Matrix expected"

   bwd :: Partial => OpBwd (Array2 (Raw Val))
   bwd (vss × Val α (Constr c (Val β1 (Int i) : Val β2 (Int j) : Nil))) | c == cPair =
      Val α (Matrix (MatrixRep (((<$>) botOf <$> vss) × (i × β1) × (j × β2)))) : Nil

matrixLookup :: ForeignOp
matrixLookup =
   ForeignOp ("!" × mkExists (ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: bwd }))
   where
   op :: OpGraph
   op (Val _ (Matrix r) : Val _ (Constr c (Val _ (Int i) : Val _ (Int j) : Nil)) : Nil) | c == cPair =
      pure $ matrixGet i j r
   op _ = throw "Matrix and pair of integers expected"

   fwd :: OpFwd (Raw MatrixRep × (Int × Int))
   fwd (Val _ (Matrix r) : Val _ (Constr c (Val _ (Int i) : Val _ (Int j) : Nil)) : Nil) | c == cPair =
      pure $ (erase r × (i × j)) × matrixGet i j r
   fwd _ = throw "Matrix and pair of integers expected"

   bwd :: OpBwd (Raw MatrixRep × (Int × Int))
   bwd ((r × (i × j)) × v) =
      Val bot (Matrix (matrixPut i j (const v) (botOf r)))
         : Val bot (Constr cPair (Val bot (Int i) : Val bot (Int j) : Nil))
         : Nil

matrixUpdate :: ForeignOp
matrixUpdate =
   ForeignOp ("matrixUpdate" × mkExists (ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val α (Matrix r) : Val _ (Constr c (Val _ (Int i) : Val _ (Int j) : Nil)) : v : Nil)
      | c == cPair = Val <$> new (singleton α) <@> Matrix (matrixPut i j (const v) r)
   op _ = throw "Matrix, pair of integers and value expected"

   fwd :: OpFwd ((Int × Int) × Raw Val)
   fwd (Val α (Matrix r) : Val _ (Constr c (Val _ (Int i) : Val _ (Int j) : Nil)) : v : Nil) | c == cPair =
      pure $ ((i × j) × erase (matrixGet i j r)) × Val α (Matrix (matrixPut i j (const v) r))
   fwd _ = throw "Matrix, pair of integers and value expected"

   bwd :: Partial => OpBwd ((Int × Int) × Raw Val)
   bwd ((((i × j) × v) × Val α (Matrix r))) =
      Val α (Matrix (matrixPut i j (const (botOf v)) r))
         : Val bot (Constr cPair (Val bot (Int i) : Val bot (Int j) : Nil))
         : matrixGet i j r
         : Nil

dict_difference :: ForeignOp
dict_difference =
   ForeignOp ("dict_difference" × mkExists (ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val α (Dictionary (DictRep d)) : Val β (Dictionary (DictRep d')) : Nil) =
      Val <$> new (singleton α # Set.insert β) <@> Dictionary (DictRep (d \\ d'))
   op _ = throw "Dictionaries expected."

   fwd :: OpFwd Unit
   fwd (Val α (Dictionary (DictRep d)) : Val α' (Dictionary (DictRep d')) : Nil) =
      pure $ unit × Val (α ∧ α') (Dictionary (DictRep (d \\ d')))
   fwd _ = throw "Dictionaries expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Val α (Dictionary d)) =
      Val α (Dictionary d) : Val α (Dictionary (DictRep empty)) : Nil

dict_fromRecord :: ForeignOp
dict_fromRecord =
   ForeignOp ("dict_fromRecord" × mkExists (ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val α (Record xvs) : Nil) = do
      xvs' <- for xvs (\v -> new (singleton α) <#> (_ × v))
      Val <$> new (singleton α) <@> Dictionary (DictRep xvs')
   op _ = throw "Record expected."

   fwd :: OpFwd Unit
   fwd (Val α (Record xvs) : Nil) =
      pure $ unit × Val α (Dictionary (DictRep $ xvs <#> (α × _)))
   fwd _ = throw "Record expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Val α (Dictionary (DictRep d))) =
      Val (foldl (∨) α (d <#> fst)) (Record (d <#> snd)) : Nil

dict_disjointUnion :: ForeignOp
dict_disjointUnion =
   ForeignOp ("dict_disjointUnion" × mkExists (ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val α (Dictionary (DictRep d)) : Val β (Dictionary (DictRep d')) : Nil) = do
      Val <$> new (singleton α # Set.insert β) <@> Dictionary (DictRep (disjointUnion d d'))
   op _ = throw "Dictionaries expected"

   fwd :: OpFwd (Dict Unit × Dict Unit)
   fwd (Val α (Dictionary (DictRep d)) : Val α' (Dictionary (DictRep d')) : Nil) =
      pure $ ((const unit <$> d) × (const unit <$> d')) × Val (α ∧ α') (Dictionary (DictRep $ disjointUnion d d'))
   fwd _ = throw "Dictionaries expected"

   bwd :: Partial => OpBwd (Dict Unit × Dict Unit)
   bwd ((d × d') × Val α (Dictionary (DictRep d''))) =
      Val α (Dictionary (DictRep (d'' \\ d'))) : Val α (Dictionary (DictRep (d'' \\ d))) : Nil

dict_foldl :: ForeignOp
dict_foldl =
   ForeignOp ("dict_foldl" × mkExists (ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (v : u : Val _ (Dictionary (DictRep d)) : Nil) =
      foldM (\u1 (_ × u2) -> G.apply v u1 >>= flip G.apply u2) u d
   op _ = throw "Function, value and dictionary expected"

   fwd :: OpFwd (Raw Val × List (String × AppTrace × AppTrace))
   fwd (v : u : Val _ (Dictionary (DictRep d)) : Nil) = do
      ts × u' <-
         foldWithIndexM
            (\s (ts × u1) (_ × u2) -> apply2 (v × u1 × u2) <#> first (\tt -> (s × tt) : ts))
            (Nil × u)
            d
      -- :: MayFail (List (String × AppTrace × AppTrace) × Val _)
      pure $ (erase v × ts) × u'
   fwd _ = throw "Function, value and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × List (String × AppTrace × AppTrace))
   bwd ((v × ts) × u) = v' : u' : Val bot (Dictionary (DictRep d)) : Nil
      where
      v' × u' × d = foldl
         ( \(v1 × u' × d) (s × tt) ->
              let v2 × u1 × u2 = apply2Bwd (tt × u') in (v1 ∨ v2) × u1 × insert s (bot × u2) d
         )
         (botOf v × u × empty)
         ts

dict_get :: ForeignOp
dict_get =
   ForeignOp ("dict_get" × mkExists (ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (Val _ (Str s) : Val _ (Dictionary (DictRep d)) : Nil) =
      snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
   op _ = throw "String and dictionary expected"

   fwd :: OpFwd String
   fwd (Val _ (Str s) : Val _ (Dictionary (DictRep d)) : Nil) =
      (s × _) <$> (snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found"))
   fwd _ = throw "String and dictionary expected"

   bwd :: Partial => OpBwd String
   bwd (s × v) =
      Val bot (Str s) : Val bot (Dictionary (DictRep $ maplet s (bot × v))) : Nil

dict_intersectionWith :: ForeignOp
dict_intersectionWith =
   ForeignOp ("dict_intersectionWith" × mkExists (ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (v : Val α (Dictionary (DictRep d1)) : Val α' (Dictionary (DictRep d2)) : Nil) =
      Val <$> new (singleton α # Set.insert α') <*> (Dictionary <$> (DictRep <$> sequence (intersectionWith apply' d1 d2)))
      where
      apply' (β × u) (β' × u') = do
         β'' <- new (singleton β # Set.insert β')
         (×) β'' <$> (G.apply v u >>= flip G.apply u')
   op _ = throw "Function and two dictionaries expected"

   fwd :: OpFwd (Raw Val × Dict (AppTrace × AppTrace))
   fwd (v : Val α (Dictionary (DictRep d)) : Val α' (Dictionary (DictRep d')) : Nil) = do
      d'' <-
         sequence $
            intersectionWith (\(β × u) (β' × u') -> (β ∧ β' × _) <$> apply2 (v × u × u')) d d'
      pure $ (erase v × (d'' <#> snd >>> fst)) × Val (α ∧ α') (Dictionary (DictRep (d'' <#> second snd)))
   fwd _ = throw "Function and two dictionaries expected"

   bwd :: Partial => OpBwd (Raw Val × Dict (AppTrace × AppTrace))
   bwd ((v × tts) × Val α (Dictionary (DictRep βvs))) =
      ( foldl (∨) (botOf v) (d' <#> (snd >>> fst))
           : Val α (Dictionary (DictRep $ d' <#> (second (snd >>> fst))))
           : Val α (Dictionary (DictRep $ d' <#> (second (snd >>> snd))))
           : Nil
      )
      where
      d' =
         intersectionWith (\tt (β × v') -> β × apply2Bwd (tt × v')) tts βvs
            :: Dict (_ × Val _ × Val _ × Val _)

dict_map :: ForeignOp
dict_map =
   ForeignOp ("dict_map" × mkExists (ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }))
   where
   op :: OpGraph
   op (v : Val α (Dictionary (DictRep d)) : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      Val <$> new (singleton α) <@> Dictionary (DictRep d')
   op _ = throw "Function and dictionary expected"

   fwd :: OpFwd (Raw Val × Dict AppTrace)
   fwd (v : Val α (Dictionary (DictRep d)) : Nil) = do
      ts × d' <- unzip <$> traverse (\(β × u) -> second (β × _) <$> apply (v × u)) d
      pure $ (erase v × ts) × Val α (Dictionary (DictRep d'))
   fwd _ = throw "Function and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × Dict AppTrace)
   bwd ((v × ts) × Val α (Dictionary (DictRep d'))) =
      (foldl (∨) (botOf v) us) : Val α (Dictionary (DictRep d)) : Nil
      where
      us × d = unzip $ intersectionWith (\t (β × u) -> second (β × _) $ applyBwd (t × u)) ts d'

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