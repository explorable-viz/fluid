module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Bind (Bind)
import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Decode (parseJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Int (ceil, floor, toNumber)
import Data.Int (quot, rem) as I
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Number (fromString)
import Data.Number (log, pow) as N
import Data.Set (Set, empty, insert)
import Data.Set as Set
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (snd)
import DataType (cCons, cNil, cPair, cTrue, cFalse)
import Debug (trace)
import Dict (fromFoldable)
import Dict (fromFoldable) as D
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Eval (apply) as G
import File (File(..), loadFileFromPath)
import Foreign.Object as FO
import Graph (Vertex)
import Graph.WithGraph (class MonadWithGraphAlloc, new)
import Lattice (class BoundedJoinSemilattice, Raw, bot)
import Prelude (div, mod) as P
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Util (type (+), type (×), Endo, definitely, definitely', error, orElse, singleton, throw, (×))
import Util.Map (disjointUnion, intersectionWith, lookup, (\\))
import Val (BaseVal(..), DictRep(..), Env, ForeignOp(..), ForeignOp'(..), Fun(..), MatrixDim(..), MatrixRep(..), Op, Val(..), matrixGet, matrixPut)

extern :: forall a. BoundedJoinSemilattice a => ForeignOp -> Bind (Val a)
extern (ForeignOp (id × φ)) =
   id × Val bot Nothing (Fun (Foreign (ForeignOp (id × φ)) Nil))

primitives :: Raw Env
primitives = wrap $ D.fromFoldable
   [ ":" × Val bot Nothing (Fun (PartialConstr cCons Nil))
   , unary "ceiling" { i: number, o: int, fwd: ceil }
   , extern debugLog
   , extern dims
   , extern error_
   , extern loadJson
   , unary "float" { i: string, o: number, fwd: definitely' <<< fromString }
   , unary "floor" { i: number, o: int, fwd: floor }
   , unary "log" { i: intOrNumber, o: number, fwd: log }
   , unary "numToStr" { i: intOrNumber, o: string, fwd: numToStr } -- rename to 'str' (more Pythonic)
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
   , extern dict_get
   , extern dict_intersectionWith
   , extern dict_map
   , extern dict
   , extern matrixUpdate
   , binaryZero "div" { i: int, o: int, fwd: div }
   , binaryZero "mod" { i: int, o: int, fwd: mod }
   , binaryZero "quot" { i: int, o: int, fwd: quot }
   , binaryZero "rem" { i: int, o: int, fwd: rem }
   ]

error_ :: ForeignOp
error_ =
   ForeignOp ("error" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (Val _ _ (Str s) : Nil) = pure $ error s
   op _ = throw "String expected"

debugLog :: ForeignOp
debugLog =
   ForeignOp ("debugLog" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (x : Nil) = pure $ trace x (const x)
   op _ = throw "Single argument expected"

loadJson :: ForeignOp
loadJson =
   ForeignOp ("loadJson" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (Val _ _ (Str path) : Nil) = do
      str <- definitely ("File \"" <> path <> "\" exists") <$> loadFileFromPath (File path)
      case parseJson str of
         Left err -> throw ("Failed to parse JSON: " <> show err)
         Right json -> fromJsonVal json
   op _ = throw "String expected"

fromJsonVal :: forall m. MonadWithGraphAlloc m => MonadEffect m => Json -> m (Val Vertex)
fromJsonVal =
   caseJson
      caseNull
      caseBool
      caseNumber
      caseString
      caseArray
      caseObject
   where
   caseNull :: Unit -> m (Val Vertex)
   caseNull _ =
      error ("Error, Null JSON value cannot be converted to Val")

   caseBool :: Boolean -> m (Val Vertex)
   caseBool b =
      new (flip Val Nothing) empty (Constr (if b then cTrue else cFalse) Nil)

   caseNumber :: Number -> m (Val Vertex)
   caseNumber n =
      case Int.fromNumber n of
         Just n' -> new (flip Val Nothing) empty (Int n')
         Nothing -> new (flip Val Nothing) empty (Float n)

   caseString :: String -> m (Val Vertex)
   caseString s =
      new (flip Val Nothing) empty (Str s)

   caseArray :: Array Json -> m (Val Vertex)
   caseArray xs = do
      vs <- traverse fromJsonVal xs
      toList (Array.toUnfoldable vs)
      where
      toList :: List (Val Vertex) -> m (Val Vertex)
      toList Nil = new (flip Val Nothing) empty (Constr cNil Nil)
      toList (v : vs) = do
         v' <- toList vs
         new (flip Val Nothing) empty (Constr cCons (v : v' : Nil))

   caseObject :: FO.Object Json -> m (Val Vertex)
   caseObject obj = do
      let kvs = FO.toUnfoldable obj :: Array (String × Json)
      entries <- for kvs \(k × x) -> do
         Val α _ _ <- new (flip Val Nothing) empty (Str k)
         v <- fromJsonVal x
         pure (k × α × v)
      new (flip Val Nothing) empty (Dictionary (DictRep (D.fromFoldable entries)))

dims :: ForeignOp
dims =
   ForeignOp ("dims" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (Val α _ (Matrix (MatrixRep (_ × MatrixDim (i × β1) × MatrixDim (j × β2)))) : Nil) = do
      v1 <- new (flip Val Nothing) (singleton β1) $ Int i
      v2 <- new (flip Val Nothing) (singleton β2) $ Int j
      let v = Constr cPair (v1 : v2 : Nil)
      new (flip Val Nothing) (singleton α) v
   op _ = throw "Matrix expected"

matrixLookup :: ForeignOp
matrixLookup =
   ForeignOp ("!" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val _ _ (Matrix r) : Val _ _ (Constr c (Val _ _ (Int i) : Val _ _ (Int j) : Nil)) : Nil) | c == cPair =
      pure $ matrixGet i j r
   op _ = throw "Matrix and pair of integers expected"

matrixUpdate :: ForeignOp
matrixUpdate =
   ForeignOp ("matrixUpdate" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (Val α _ (Matrix r) : Val _ _ (Constr c (Val _ _ (Int i) : Val _ _ (Int j) : Nil)) : v : Nil)
      | c == cPair = new (flip Val Nothing) (singleton α) (Matrix (matrixPut i j (const v) r))
   op _ = throw "Matrix, pair of integers and value expected"

dict_difference :: ForeignOp
dict_difference =
   ForeignOp ("dict_difference" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) =
      new (flip Val Nothing) (singleton α # Set.insert β) (Dictionary (DictRep (d \\ d')))
   op _ = throw "Dictionaries expected."

dict_disjointUnion :: ForeignOp
dict_disjointUnion =
   ForeignOp ("dict_disjointUnion" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) = do
      new (flip Val Nothing) (singleton α # Set.insert β) (Dictionary (DictRep (disjointUnion d d')))
   op _ = throw "Dictionaries expected"

dict_foldl :: ForeignOp
dict_foldl =
   ForeignOp ("dict_foldl" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (v : u : Val _ _ (Dictionary (DictRep d)) : Nil) =
      foldM (\u1 (_ × u2) -> G.apply v u1 >>= flip G.apply u2) u d
   op _ = throw "Function, value and dictionary expected"

dict_get :: ForeignOp
dict_get =
   ForeignOp ("dict_get" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val _ _ (Str s) : Val _ _ (Dictionary (DictRep d)) : Nil) =
      snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
   op _ = throw "String and dictionary expected"

dict_intersectionWith :: ForeignOp
dict_intersectionWith =
   ForeignOp ("dict_intersectionWith" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d1)) : Val α' _ (Dictionary (DictRep d2)) : Nil) = do
      v' <- Dictionary <$> (DictRep <$> sequence (intersectionWith apply' d1 d2))
      new (flip Val Nothing) (singleton α # Set.insert α') v'
      where
      apply' (β × u) (β' × u') = do
         v''@(Val _ _ key) <- G.apply v u >>= flip G.apply u'
         Val β'' _ _ <- new (flip Val Nothing) (singleton β # Set.insert β') key
         pure (β'' × v'')
   op _ = throw "Function and two dictionaries expected"

dict_map :: ForeignOp
dict_map =
   ForeignOp ("dict_map" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d)) : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      new (flip Val Nothing) (singleton α) (Dictionary (DictRep d'))
   op _ = throw "Function and dictionary expected"

dict :: ForeignOp
dict =
   ForeignOp ("dict" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (v : Nil) = do
      αs × kvs <- kvs' v
      new (flip Val Nothing) αs (Dictionary (DictRep $ fromFoldable kvs))
      where
      kvs' :: forall m. MonadError Error m => Val Vertex -> m (Set Vertex × List (String × (Vertex × Val Vertex)))
      kvs' (Val α _ (Constr c Nil)) | c == cNil = pure $ singleton α × Nil
      kvs' (Val α _ (Constr c (Val β' _ (Constr c' (Val β _ (Str k) : u : Nil)) : v' : Nil)))
         | c == cCons && c' == cPair = do
              αs' × kvs <- kvs' v'
              pure $ insert α (insert β' αs') × ((k × (β × u)) : kvs)
      kvs' _ = throw $ "List of (key, value) pairs expected"
   op _ = throw "Single argument expected"

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
