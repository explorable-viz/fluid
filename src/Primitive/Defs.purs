module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Data.Exists (mkExists)
import Data.Foldable (foldl, foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Profunctor.Strong (first, second)
import Data.Set (empty, insert, singleton)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict, (\\))
import Dict (disjointUnion, empty, fromFoldable, insert, intersectionWith, lookup, singleton, unzip) as D
import Eval (apply, apply2)
import EvalBwd (apply2Bwd, applyBwd)
import EvalGraph (apply) as G
import Graph.GraphWriter (new)
import Lattice (class BoundedJoinSemilattice, Raw, bot, botOf, erase, top, (∧), (∨))
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Trace (AppTrace)
import Util (type (+), type (×), Endo, error, orElse, throw, unimplemented, (×))
import Val (Array2, DictRep(..), Env, ForeignOp, ForeignOp'(..), Fun(..), MatrixRep(..), OpBwd, OpFwd, OpGraph, Val(..), matrixGet, matrixUpdate)

extern :: forall a. BoundedJoinSemilattice a => ForeignOp -> Val a
extern = Fun bot <<< flip Foreign Nil

primitives :: Raw Env
primitives = D.fromFoldable
   [ ":" × Fun bot (PartialConstr cCons Nil)
   , "ceiling" × unary { i: number, o: int, fwd: ceil }
   , "debugLog" × extern debugLog
   , "dims" × extern dims
   , "error" × extern error_
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
   , "dict_foldl" × extern dict_foldl
   , "dict_fromRecord" × extern dict_fromRecord
   , "dict_get" × extern dict_get
   , "dict_intersectionWith" × extern dict_intersectionWith
   , "dict_map" × extern dict_map
   , "div" × binaryZero { i: int, o: int, fwd: div }
   , "mod" × binaryZero { i: int, o: int, fwd: mod }
   , "quot" × binaryZero { i: int, o: int, fwd: quot }
   , "rem" × binaryZero { i: int, o: int, fwd: rem }
   , "matrixUpdate" × extern matrixMut
   ]

error_ :: ForeignOp
error_ = mkExists $ ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }
   where
   op' :: OpGraph
   op' (Str _ s : Nil) = pure $ error s
   op' _ = throw "String expected"

   fwd :: OpFwd Unit
   fwd (Str _ s : Nil) = error s
   fwd _ = throw "String expected"

   bwd :: OpBwd Unit
   bwd _ = error unimplemented

debugLog :: ForeignOp
debugLog = mkExists $ ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }
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
dims = mkExists $ ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Matrix α (MatrixRep (_ × (i × β1) × (j × β2))) : Nil) = do
      v1 <- Int <$> new (singleton β1) <@> i
      v2 <- Int <$> new (singleton β2) <@> j
      Constr <$> new (singleton α) <@> cPair <@> (v1 : v2 : Nil)
   op _ = throw "Matrix expected"

   fwd :: OpFwd (Array2 (Raw Val))
   fwd (Matrix α (MatrixRep (vss × (i × β1) × (j × β2))) : Nil) =
      pure $ (map erase <$> vss) × Constr α cPair (Int β1 i : Int β2 j : Nil)
   fwd _ = throw "Matrix expected"

   bwd :: Partial => OpBwd (Array2 (Raw Val))
   bwd (vss × Constr α c (Int β1 i : Int β2 j : Nil)) | c == cPair =
      Matrix α (MatrixRep (((<$>) botOf <$> vss) × (i × β1) × (j × β2))) : Nil

matrixLookup :: ForeignOp
matrixLookup = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: bwd }
   where
   op :: OpGraph
   op (Matrix _ r : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = pure $ matrixGet i j r
   op _ = throw "Matrix and pair of integers expected"

   fwd :: OpFwd (Raw MatrixRep × (Int × Int))
   fwd (Matrix _ r@(MatrixRep (vss × (i' × _) × (j' × _))) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = do
           let v = matrixGet i j r
           pure $ (MatrixRep ((map erase <$> vss) × ((i' × unit) × (j' × unit))) × (i × j)) × v
   fwd _ = throw "Matrix and pair of integers expected"

   bwd :: OpBwd (Raw MatrixRep × (Int × Int))
   bwd (((MatrixRep (vss × (i' × _) × (j' × _))) × (i × j)) × v) =
      Matrix bot (matrixUpdate i j (const v) (MatrixRep (((<$>) botOf <$> vss) × (i' × bot) × (j' × bot))))
         : Constr bot cPair (Int bot i : Int bot j : Nil)
         : Nil

matrixMut :: ForeignOp
matrixMut = mkExists $ ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: bwd }
   where
   op :: OpGraph
   op (Matrix _ r : Constr _ c (Int _ i : Int _ j : Nil) : v : Nil)
      | c == cPair = Matrix <$> new empty <@> (matrixUpdate i j (const v) r)
   op _ = throw "Matrix, pair of ints, and new val expected"

   fwd :: OpFwd (Raw MatrixRep × (Int × Int) × Raw Val)
   fwd (Matrix _ r@(MatrixRep (vss × (i' × _) × (j' × _))) : Constr _ c (Int _ i : Int _ j : Nil) : v : Nil)
      | c == cPair =
           let
              oldV = matrixGet i j r
              newM = matrixUpdate i j (const v) r
           in
              pure $ (MatrixRep ((map erase <$> vss) × ((i' × unit) × (j' × unit))) × (i × j) × (erase oldV)) × (Matrix top newM)

   fwd _ = throw "Matrix, pair of ints, and new val expected"

   bwd :: OpBwd (Raw MatrixRep × (Int × Int) × Raw Val)
   bwd ((((MatrixRep (vss × (i' × _) × (j' × _))) × (i × j) × oldV) × (Matrix _ r))) =
      let
         newV = matrixGet i j r
      in
         Matrix bot (matrixUpdate i j (const (map (const bot) oldV)) (MatrixRep (((<$>) botOf <$> vss) × (i' × bot) × (j' × bot))))
            : Constr bot cPair (Int bot i : Int bot j : Nil)
            : newV
            : Nil
   bwd _ = error "absurd backwards!"

dict_difference :: ForeignOp
dict_difference = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Dictionary α (DictRep d) : Dictionary β (DictRep d') : Nil) =
      Dictionary <$> new (singleton α # insert β) <@> DictRep (d \\ d')
   op _ = throw "Dictionaries expected."

   fwd :: OpFwd Unit
   fwd (Dictionary α (DictRep d) : Dictionary α' (DictRep d') : Nil) =
      pure $ unit × Dictionary (α ∧ α') (DictRep (d \\ d'))
   fwd _ = throw "Dictionaries expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α d) =
      Dictionary α d : Dictionary α (DictRep D.empty) : Nil

dict_fromRecord :: ForeignOp
dict_fromRecord = mkExists $ ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Record α xvs : Nil) = do
      xvs' <- for xvs (\v -> new (singleton α) <#> (_ × v))
      Dictionary <$> new (singleton α) <@> DictRep xvs'
   op _ = throw "Record expected."

   fwd :: OpFwd Unit
   fwd (Record α xvs : Nil) =
      pure $ unit × Dictionary α (DictRep $ xvs <#> (α × _))
   fwd _ = throw "Record expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α (DictRep d)) =
      Record (foldl (∨) α (d <#> fst)) (d <#> snd) : Nil

dict_disjointUnion :: ForeignOp
dict_disjointUnion = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Dictionary α (DictRep d) : Dictionary β (DictRep d') : Nil) = do
      Dictionary <$> new (singleton α # insert β) <@> DictRep (D.disjointUnion d d')
   op _ = throw "Dictionaries expected"

   fwd :: OpFwd (Dict Unit × Dict Unit)
   fwd (Dictionary α (DictRep d) : Dictionary α' (DictRep d') : Nil) =
      pure $ ((const unit <$> d) × (const unit <$> d')) × Dictionary (α ∧ α') (DictRep $ D.disjointUnion d d')
   fwd _ = throw "Dictionaries expected"

   bwd :: Partial => OpBwd (Dict Unit × Dict Unit)
   bwd ((d × d') × Dictionary α (DictRep d'')) =
      Dictionary α (DictRep (d'' \\ d')) : Dictionary α (DictRep (d'' \\ d)) : Nil

dict_foldl :: ForeignOp
dict_foldl = mkExists $ ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (v : u : Dictionary _ (DictRep d) : Nil) =
      foldM (\u1 (_ × u2) -> G.apply v u1 >>= flip G.apply u2) u d
   op _ = throw "Function, value and dictionary expected"

   fwd :: OpFwd (Raw Val × List (String × AppTrace × AppTrace))
   fwd (v : u : Dictionary _ (DictRep d) : Nil) = do
      ts × u' <-
         foldWithIndexM
            (\s (ts × u1) (_ × u2) -> apply2 (v × u1 × u2) <#> first (\tt -> (s × tt) : ts))
            (Nil × u)
            d
      -- :: MayFail (List (String × AppTrace × AppTrace) × Val _)
      pure $ (erase v × ts) × u'
   fwd _ = throw "Function, value and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × List (String × AppTrace × AppTrace))
   bwd ((v × ts) × u) = v' : u' : Dictionary bot (DictRep d) : Nil
      where
      v' × u' × d = foldl
         ( \(v1 × u' × d) (s × tt) ->
              let v2 × u1 × u2 = apply2Bwd (tt × u') in (v1 ∨ v2) × u1 × D.insert s (bot × u2) d
         )
         (botOf v × u × D.empty)
         ts

dict_get :: ForeignOp
dict_get = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Str _ s : Dictionary _ (DictRep d) : Nil) =
      snd <$> D.lookup s d # orElse ("Key \"" <> s <> "\" not found")
   op _ = throw "String and dictionary expected"

   fwd :: OpFwd String
   fwd (Str _ s : Dictionary _ (DictRep d) : Nil) =
      (s × _) <$> (snd <$> D.lookup s d # orElse ("Key \"" <> s <> "\" not found"))
   fwd _ = throw "String and dictionary expected"

   bwd :: Partial => OpBwd String
   bwd (s × v) =
      Str bot s : Dictionary bot (DictRep $ D.singleton s (bot × v)) : Nil

dict_intersectionWith :: ForeignOp
dict_intersectionWith = mkExists $ ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (v : Dictionary α (DictRep d1) : Dictionary α' (DictRep d2) : Nil) =
      Dictionary <$> new (singleton α # insert α')
         <*> (DictRep <$> sequence (D.intersectionWith apply' d1 d2))
      where
      apply' (β × u) (β' × u') = do
         β'' <- new (singleton β # insert β')
         (×) β'' <$> (G.apply v u >>= flip G.apply u')
   op _ = throw "Function and two dictionaries expected"

   fwd :: OpFwd (Raw Val × Dict (AppTrace × AppTrace))
   fwd (v : Dictionary α (DictRep d) : Dictionary α' (DictRep d') : Nil) = do
      d'' <-
         sequence $
            D.intersectionWith (\(β × u) (β' × u') -> (β ∧ β' × _) <$> apply2 (v × u × u')) d d'
      pure $ (erase v × (d'' <#> snd >>> fst)) × Dictionary (α ∧ α') (DictRep (d'' <#> second snd))
   fwd _ = throw "Function and two dictionaries expected"

   bwd :: Partial => OpBwd (Raw Val × Dict (AppTrace × AppTrace))
   bwd ((v × tts) × Dictionary α (DictRep βvs)) =
      ( foldl (∨) (botOf v) (d' <#> (snd >>> fst))
           : Dictionary α (DictRep $ d' <#> (second (snd >>> fst)))
           : Dictionary α (DictRep $ d' <#> (second (snd >>> snd)))
           : Nil
      )
      where
      d' =
         D.intersectionWith (\tt (β × v') -> β × apply2Bwd (tt × v')) tts βvs
            :: Dict (_ × Val _ × Val _ × Val _)

dict_map :: ForeignOp
dict_map = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (v : Dictionary α (DictRep d) : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      Dictionary <$> new (singleton α) <@> DictRep d'
   op _ = throw "Function and dictionary expected"

   fwd :: OpFwd (Raw Val × Dict AppTrace)
   fwd (v : Dictionary α (DictRep d) : Nil) = do
      ts × d' <- D.unzip <$> traverse (\(β × u) -> second (β × _) <$> apply (v × u)) d
      pure $ (erase v × ts) × Dictionary α (DictRep d')
   fwd _ = throw "Function and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × Dict AppTrace)
   bwd ((v × ts) × Dictionary α (DictRep d')) =
      (foldl (∨) (botOf v) us) : Dictionary α (DictRep d) : Nil
      where
      us × d = D.unzip $ D.intersectionWith (\t (β × u) -> second (β × _) $ applyBwd (t × u)) ts d'

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
