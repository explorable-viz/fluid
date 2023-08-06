module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Control.Monad.Except (except)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Exists (mkExists)
import Data.Foldable (foldl, foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.List (List(..), (:))
import Data.Number (log, pow) as N
import Data.Profunctor.Strong (first, second)
import Data.Set (insert, singleton)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (cCons, cPair)
import Debug (trace)
import Dict (Dict, (\\))
import Dict (disjointUnion, empty, fromFoldable, insert, intersectionWith, lookup, singleton, unzip) as D
import Eval (apply, apply2)
import EvalBwd (apply2Bwd, applyBwd)
import EvalGraph (apply) as G
import Graph (new)
import Lattice (Raw, (∨), (∧), bot, botOf, erase)
import Partial.Unsafe (unsafePartial)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Trace (AppTrace)
import Util (type (+), type (×), Endo, MayFail, error, orElse, report, unimplemented, (×))
import Val (Array2, Env, ForeignOp, ForeignOp'(..), Fun(..), OpBwd, OpFwd, OpGraph, Val(..), updateMatrix)

extern :: forall a. ForeignOp -> Val a
extern = flip Foreign Nil >>> Fun

primitives :: Raw Env
primitives = D.fromFoldable
   [ ":" × Fun (PartialConstr bot cCons Nil)
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
   ]

error_ :: ForeignOp
error_ = mkExists $ ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }
   where
   op' :: OpGraph
   op' (Str _ s : Nil) = pure $ error s
   op' _ = except $ report "String expected"

   fwd :: OpFwd Unit
   fwd (Str _ s : Nil) = error s
   fwd _ = report "String expected"

   bwd :: OpBwd Unit
   bwd _ = error unimplemented

debugLog :: ForeignOp
debugLog = mkExists $ ForeignOp' { arity: 1, op': op', op: fwd, op_bwd: unsafePartial bwd }
   where
   op' :: OpGraph
   op' (x : Nil) = pure $ trace x (const x)
   op' _ = except $ report "Single value expected"

   fwd :: OpFwd Unit
   fwd (x : Nil) = pure $ unit × trace x (const x)
   fwd _ = report "Single value expected"

   bwd :: OpBwd Unit
   bwd _ = error unimplemented

type ArrayData a = Array2 (Val a)

dims :: ForeignOp
dims = mkExists $ ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Matrix α (_ × (i × β1) × (j × β2)) : Nil) = do
      v1 <- Int <$> lift (new (singleton β1)) <@> i
      v2 <- Int <$> lift (new (singleton β2)) <@> j
      Constr <$> lift (new (singleton α)) <@> cPair <@> (v1 : v2 : Nil)
   op _ = except $ report "Matrix expected"

   fwd :: OpFwd (Raw ArrayData)
   fwd (Matrix α (vss × (i × β1) × (j × β2)) : Nil) =
      pure $ (map erase <$> vss) × Constr α cPair (Int β1 i : Int β2 j : Nil)
   fwd _ = report "Matrix expected"

   bwd :: Partial => OpBwd (Raw ArrayData)
   bwd (vss × Constr α c (Int β1 i : Int β2 j : Nil)) | c == cPair =
      Matrix α (((<$>) botOf <$> vss) × (i × β1) × (j × β2)) : Nil

matrixLookup :: ForeignOp
matrixLookup = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: bwd }
   where
   op :: OpGraph
   op (Matrix _ (vss × _ × _) : Constr _ c (Int _ i : Int _ j : Nil) : Nil)
      | c == cPair = do
           v <- except $ orElse "Index out of bounds" $ do
              us <- vss !! (i - 1)
              us !! (j - 1)
           pure v
   op _ = except $ report "Matrix and pair of integers expected"

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
dict_difference = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Dictionary α d : Dictionary β d' : Nil) =
      Dictionary <$> lift (new (singleton α # insert β)) <@> (d \\ d')
   op _ = except $ report "Dictionaries expected."

   fwd :: OpFwd Unit
   fwd (Dictionary α d : Dictionary α' d' : Nil) =
      pure $ unit × Dictionary (α ∧ α') (d \\ d')
   fwd _ = report "Dictionaries expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α d) =
      Dictionary α d : Dictionary α D.empty : Nil

dict_fromRecord :: ForeignOp
dict_fromRecord = mkExists $ ForeignOp' { arity: 1, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Record α xvs : Nil) = do
      xvs' <- for xvs (\v -> lift (new (singleton α)) <#> (_ × v))
      Dictionary <$> lift (new (singleton α)) <@> xvs'
   op _ = except $ report "Record expected."

   fwd :: OpFwd Unit
   fwd (Record α xvs : Nil) =
      pure $ unit × Dictionary α (xvs <#> (α × _))
   fwd _ = report "Record expected."

   bwd :: Partial => OpBwd Unit
   bwd (_ × Dictionary α d) = Record (foldl (∨) α (d <#> fst)) (d <#> snd) : Nil

dict_disjointUnion :: ForeignOp
dict_disjointUnion = mkExists $ ForeignOp' { arity: 2, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (Dictionary α d : Dictionary β d' : Nil) = do
      Dictionary <$> lift (new (singleton α # insert β)) <@> D.disjointUnion d d'
   op _ = except $ report "Dictionaries expected"

   fwd :: OpFwd (Dict Unit × Dict Unit)
   fwd (Dictionary α d : Dictionary α' d' : Nil) =
      pure $ ((const unit <$> d) × (const unit <$> d')) × Dictionary (α ∧ α') (D.disjointUnion d d')
   fwd _ = report "Dictionaries expected"

   bwd :: Partial => OpBwd (Dict Unit × Dict Unit)
   bwd ((d × d') × Dictionary α d'') =
      Dictionary α (d'' \\ d') : Dictionary α (d'' \\ d) : Nil

dict_foldl :: ForeignOp
dict_foldl = mkExists $ ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (v : u : Dictionary _ d : Nil) =
      foldM (\u1 (_ × u2) -> G.apply v u1 >>= flip G.apply u2) u d
   op _ = except $ report "Function, value and dictionary expected"

   fwd :: OpFwd (Raw Val × List (String × AppTrace × AppTrace))
   fwd (v : u : Dictionary _ d : Nil) = do
      ts × u' <-
         foldWithIndexM
            (\s (ts × u1) (_ × u2) -> apply2 (v × u1 × u2) <#> first (\tt -> (s × tt) : ts))
            (Nil × u)
            d
            :: MayFail (List (String × AppTrace × AppTrace) × Val _)
      pure $ (erase v × ts) × u'
   fwd _ = report "Function, value and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × List (String × AppTrace × AppTrace))
   bwd ((v × ts) × u) = v' : u' : Dictionary bot d : Nil
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
   op (Str _ s : Dictionary _ d : Nil) =
      snd <$> except (D.lookup s d # orElse ("Key \"" <> s <> "\" not found"))
   op _ = except $ report "String and dictionary expected"

   fwd :: OpFwd String
   fwd (Str _ s : Dictionary _ d : Nil) =
      (s × _) <$> (snd <$> D.lookup s d # orElse ("Key \"" <> s <> "\" not found"))
   fwd _ = report "String and dictionary expected"

   bwd :: Partial => OpBwd String
   bwd (s × v) =
      Str bot s : Dictionary bot (D.singleton s (bot × v)) : Nil

dict_intersectionWith :: ForeignOp
dict_intersectionWith = mkExists $ ForeignOp' { arity: 3, op': op, op: fwd, op_bwd: unsafePartial bwd }
   where
   op :: OpGraph
   op (v : Dictionary α d1 : Dictionary α' d2 : Nil) =
      Dictionary <$> lift (new (singleton α # insert α')) <*> sequence (D.intersectionWith apply' d1 d2)
      where
      apply' (β × u) (β' × u') = do
         β'' <- lift $ new (singleton β # insert β')
         (×) β'' <$> (G.apply v u >>= flip G.apply u')
   op _ = except $ report "Function and two dictionaries expected"

   fwd :: OpFwd (Raw Val × Dict (AppTrace × AppTrace))
   fwd (v : Dictionary α d : Dictionary α' d' : Nil) = do
      d'' <-
         sequence $
            D.intersectionWith (\(β × u) (β' × u') -> (β ∧ β' × _) <$> apply2 (v × u × u')) d d'
            :: MayFail (Dict (_ × (AppTrace × AppTrace) × Val _))
      pure $ (erase v × (d'' <#> snd >>> fst)) × Dictionary (α ∧ α') (d'' <#> second snd)
   fwd _ = report "Function and two dictionaries expected"

   bwd :: Partial => OpBwd (Raw Val × Dict (AppTrace × AppTrace))
   bwd ((v × tts) × Dictionary α βvs) =
      ( foldl (∨) (botOf v) (d' <#> (snd >>> fst))
           : Dictionary α (d' <#> (second (snd >>> fst)))
           : Dictionary α (d' <#> (second (snd >>> snd)))
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
   op (v : Dictionary α d : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      Dictionary <$> lift (new (singleton α)) <@> d'
   op _ = except $ report "Function and dictionary expected"

   fwd :: OpFwd (Raw Val × Dict AppTrace)
   fwd (v : Dictionary α d : Nil) = do
      ts × d' <- D.unzip <$> traverse (\(β × u) -> second (β × _) <$> apply (v × u)) d
      pure $ (erase v × ts) × Dictionary α d'
   fwd _ = report "Function and dictionary expected"

   bwd :: Partial => OpBwd (Raw Val × Dict AppTrace)
   bwd ((v × ts) × Dictionary α d') =
      (foldl (∨) (botOf v) us) : Dictionary α d : Nil
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
