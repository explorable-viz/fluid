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
import Data.Set (Set, empty)
import Data.Set as Set
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for, sequence, traverse)
import DataType (cCons, cFalse, cNil, cNone, cPair, cSome, cTrue)
import Debug (trace)
import Dict (fromFoldable)
import Dict (fromFoldable) as D
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Eval (apply) as G
import File (File(..), loadFileFromPath)
import Foreign.Object as FO
import Graph (Vertex)
import Graph.WithGraph (class MonadWithGraphAlloc)
import Lattice (class BoundedJoinSemilattice, Raw, bot)
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Util (type (+), type (×), Endo, definitely, definitely', error, singleton, throw, (×))
import Util.Map (disjointUnion, intersectionWith, lookup, (\\))
import Util.Map as Dict
import Util.Map as Map
import Val (BaseVal(..), DictRep(..), Env, ForeignOp(..), ForeignOp'(..), Fun(..), MatrixDim(..), MatrixRep(..), Op, Val(..), matrixGet, matrixPut, val)

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
   , unary "num_to_str" { i: intOrNumber, o: string, fwd: numToStr } -- rename to 'str' (more Pythonic)
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
   -- TODO: rename the rest of these (apart from dict_map?) to lose the dict_ prefix
   , extern dict_difference
   , extern dict_disjointUnion
   , extern foldl_with_index
   , extern get
   , extern insert
   , extern dict_intersectionWith
   , extern dict_map
   , extern dict
   , extern matrixUpdate
   , extern search
   , extern split
   , binaryZero "//" { i: int, o: int, fwd: div }
   , binaryZero "%" { i: int, o: int, fwd: mod }
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
      val empty (Constr (if b then cTrue else cFalse) Nil)

   caseNumber :: Number -> m (Val Vertex)
   caseNumber n =
      case Int.fromNumber n of
         Just n' -> val empty (Int n')
         Nothing -> val empty (Float n)

   caseString :: String -> m (Val Vertex)
   caseString s =
      val empty (Str s)

   caseArray :: Array Json -> m (Val Vertex)
   caseArray xs = do
      vs <- traverse fromJsonVal xs
      toList (Array.toUnfoldable vs)
      where
      toList :: List (Val Vertex) -> m (Val Vertex)
      toList Nil = val empty (Constr cNil Nil)
      toList (v : vs) = do
         v' <- toList vs
         val empty (Constr cCons (v : v' : Nil))

   caseObject :: FO.Object Json -> m (Val Vertex)
   caseObject obj = do
      let kvs = FO.toUnfoldable obj :: Array (String × Json)
      entries <- for kvs \(k × x) -> do
         Val α _ _ <- val empty (Str k)
         v <- fromJsonVal x
         pure (k × α × v)
      val empty (Dictionary (DictRep (D.fromFoldable entries)))

dims :: ForeignOp
dims =
   ForeignOp ("dims" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (Val α _ (Matrix (MatrixRep (_ × MatrixDim (i × β1) × MatrixDim (j × β2)))) : Nil) = do
      v1 <- val (singleton β1) $ Int i
      v2 <- val (singleton β2) $ Int j
      let v = Constr cPair (v1 : v2 : Nil)
      val (singleton α) v
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
      | c == cPair = val (singleton α) (Matrix (matrixPut i j (const v) r))
   op _ = throw "Matrix, pair of integers and value expected"

search :: ForeignOp
search =
   ForeignOp ("search" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Str regex) : Val β _ (Str str) : Nil) = do
      case Regex.regex regex noFlags of
         Left msg -> throw $ "search: " <> msg
         Right regex' -> do
            let αs = singleton α # Set.insert β
            case Regex.search regex' str of
               Nothing -> val αs (Constr cNone Nil)
               Just n -> do
                  v <- val αs (Int n)
                  val αs (Constr cSome (v : Nil))
   op _ = throw "Regex and string expected"

-- When strings implement an abstract sequence type can express in terms of take/drop
split :: ForeignOp
split =
   ForeignOp ("search" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Int n) : Val β _ (Str str) : Nil) = do
      let αs = singleton α # Set.insert β
      before <- val αs $ Str $ String.take n str
      after <- val αs $ Str $ String.drop n str
      val αs (Constr cPair (before : after : Nil))
   op _ = throw "Int and string expected"

dict_difference :: ForeignOp
dict_difference =
   ForeignOp ("dict_difference" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) =
      val (singleton α # Set.insert β) (Dictionary (DictRep (d \\ d')))
   op _ = throw "Dictionaries expected."

dict_disjointUnion :: ForeignOp
dict_disjointUnion =
   ForeignOp ("dict_disjointUnion" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) = do
      val (singleton α # Set.insert β) (Dictionary (DictRep (disjointUnion d d')))
   op _ = throw "Dictionaries expected"

foldl_with_index :: ForeignOp
foldl_with_index =
   ForeignOp ("foldl_with_index" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (v : u : Val _ _ (Dictionary (DictRep d)) : Nil) =
      foldM (\u1 (k × (α × u2)) -> G.apply v (Val α Nothing (Str k)) >>= flip G.apply u1 >>= flip G.apply u2) u kvs
      where
      kvs :: List _
      kvs = Dict.toUnfoldable d
   op _ = throw "Function, value and dictionary expected"

get :: ForeignOp
get =
   ForeignOp ("get" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (Val α _ (Str s) : Val _ _ (Dictionary (DictRep d)) : Nil) =
      case lookup s d of
         Nothing -> val (singleton α) (Constr cNone Nil)
         Just (β × v) -> val (Set.insert β (singleton α)) (Constr cSome (v : Nil))
   op _ = throw "String and dictionary expected"

insert :: ForeignOp
insert =
   ForeignOp ("insert" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val α' _ (Str k) : v : Nil) =
      val (singleton α) (Dictionary (DictRep (Map.insert k (α' × v) d)))
   op _ = throw "Dictionary, key and value expected"

dict_intersectionWith :: ForeignOp
dict_intersectionWith =
   ForeignOp ("dict_intersectionWith" × ForeignOp' { arity: 3, op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d1)) : Val α' _ (Dictionary (DictRep d2)) : Nil) = do
      v' <- Dictionary <$> (DictRep <$> sequence (intersectionWith apply' d1 d2))
      val (singleton α # Set.insert α') v'
      where
      apply' (β × u) (β' × u') = do
         v''@(Val _ _ key) <- G.apply v u >>= flip G.apply u'
         Val β'' _ _ <- val (singleton β # Set.insert β') key
         pure (β'' × v'')
   op _ = throw "Function and two dictionaries expected"

dict_map :: ForeignOp
dict_map =
   ForeignOp ("dict_map" × ForeignOp' { arity: 2, op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d)) : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      val (singleton α) (Dictionary (DictRep d'))
   op _ = throw "Function and dictionary expected"

dict :: ForeignOp
dict =
   ForeignOp ("dict" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (v : Nil) = do
      αs × kvs <- kvs' v
      val αs (Dictionary (DictRep $ fromFoldable kvs))
      where
      kvs' :: forall m. MonadError Error m => Val Vertex -> m (Set Vertex × List (String × (Vertex × Val Vertex)))
      kvs' (Val α _ (Constr c Nil)) | c == cNil = pure $ singleton α × Nil
      kvs' (Val α _ (Constr c (Val β' _ (Constr c' (Val β _ (Str k) : u : Nil)) : v' : Nil)))
         | c == cCons && c' == cPair = do
              αs' × kvs <- kvs' v'
              pure $ Set.insert α (Set.insert β' αs') × ((k × (β × u)) : kvs)
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
-- and https://github.com/explorable-viz/fluid/issues/1450
div :: Int -> Endo Int
div = (\x y -> floor (toNumber x / toNumber y))

mod :: Int -> Endo Int
mod = (\x y -> x - y * div x y)

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
