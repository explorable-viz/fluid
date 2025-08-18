module Primitive.Defs where

import Prelude hiding (absurd, apply, div, mod, top)

import Bind (Bind)
import Control.Monad.Reader (ask)
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
import Data.Number (log, pow) as N
import Data.Set (empty, fromFoldable)
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import DataType (cCons, cNil, cPair, cTrue, cFalse)
import Debug (trace)
import Dict (fromFoldable) as D
import Doc (DocOpt(..))
import EvalGraph (apply) as G
import File (File(..), FileCxt(..), loadFile)
import Foreign.Object as FO
import Graph (Vertex)
import Graph.WithGraph (class MonadWithGraphAlloc, new)
import Lattice (class BoundedJoinSemilattice, Raw, bot)
import Prelude (div, mod) as P
import Pretty (pretty)
import Primitive (binary, binaryZero, boolean, int, intOrNumber, intOrNumberOrString, number, string, unary, union, union1, unionStr)
import Util (type (+), Endo, error, orElse, singleton, spy, throw, (×))
import Util.Map (disjointUnion, intersectionWith, lookup, (\\))
import Util.Pretty (render)
import Val (BaseVal(..), DictRep(..), Env, ForeignOp(..), ForeignOp'(..), Fun(..), MatrixDim(..), MatrixRep(..), Op, Val(..), matrixGet, matrixPut)

extern :: forall a. BoundedJoinSemilattice a => ForeignOp -> Bind (Val a)
extern (ForeignOp (id × φ)) = id × Val bot None (Fun ((Foreign (ForeignOp (id × φ))) Nil))

primitives :: Raw Env
primitives = wrap $ D.fromFoldable
   [ ":" × Val bot None (Fun (PartialConstr cCons Nil))
   , unary "ceiling" { i: number, o: int, fwd: ceil }
   , extern debugLog
   , extern dims
   , extern error_
   , extern loadJson
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
   ForeignOp ("error" × ForeignOp' { arity: 1, op: op })
   where
   op :: Op
   op (Val _ _ (Str s) : Nil) = pure $ error s
   op _ = throw "String expected"

debugLog :: ForeignOp
debugLog =
   ForeignOp ("debugLog" × ForeignOp' { arity: 1, op: op })
   where
   op :: Op
   op (x : Nil) = pure $ trace x (const x)
   op _ = throw "Single value expected"

loadJson :: ForeignOp
loadJson =
   ForeignOp ("loadJson" × ForeignOp' { arity: 1, op })
   where
   op :: Op
   op (Val _ _ (Str path) : Nil) = do
      FileCxt { fluidSrcPaths } <- ask
      str <- loadFile fluidSrcPaths (File path)
      case parseJson str of
         Left err -> throw ("Failed to parse JSON: " <> show err)
         Right (j :: Json) ->
            fromJsonVal' j
   op _ = throw "String expected"

fromJsonVal' :: forall m. MonadWithGraphAlloc m => Json -> m (Val Vertex)
fromJsonVal' =
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
      error ("Error, Null JSON value cannot be converted to Val Vertex")

   caseBool :: Boolean -> m (Val Vertex)
   caseBool b =
      new (flip Val None) empty (Constr (if b then cTrue else cFalse) Nil)

   caseNumber :: Number -> m (Val Vertex)
   caseNumber n =
      case Int.fromNumber n of
         Just n' -> new (flip Val None) empty (Int n')
         Nothing -> new (flip Val None) empty (Float n)

   caseString :: String -> m (Val Vertex)
   caseString s =
      new (flip Val None) empty (Str s)

   caseArray :: Array Json -> m (Val Vertex)
   caseArray arr = do
      vs <- traverse fromJsonVal' arr
      v <- arrVtoVal' (Array.toUnfoldable vs :: List (Val Vertex))
      pure (spy "Processing array" (render <<< pretty) v)

   caseObject :: FO.Object Json -> m (Val Vertex)
   caseObject obj = do
      let kvs = FO.toUnfoldable obj :: Array (Tuple String Json)
      entries <- traverse
         ( \(Tuple k vj) -> do
              vv <- fromJsonVal' vj
              pure (Tuple k (addr vv × vv))
         )
         kvs
      let
         d = D.fromFoldable entries
         deps = Set.fromFoldable (entries <#> \(Tuple _ (β × _)) -> β)
      new (flip Val None) deps (Dictionary (DictRep d))

-- build a Fluid list node-by-node, allocating each Cons/Nil and wiring edges to children
arrVtoVal' :: forall m. MonadWithGraphAlloc m => List (Val Vertex) -> m (Val Vertex)
arrVtoVal' Nil = new (flip Val None) empty (Constr cNil Nil)
arrVtoVal' (x : xs) = do
   tailV <- arrVtoVal' xs
   let
      deps = fromFoldable [ addr x, addr tailV ]
   new (flip Val None) deps (Constr cCons (x : tailV : Nil))

-- take the nodes vertex address and return the address of the node
addr :: Val Vertex -> Vertex
addr (Val α _ _) = α

-- fromJsonVal' =
--    caseJson
-- ( \_ ->
--      throw "Error, Null JSON value cannot be converted to Val Vertex"
-- )
--       ( \b -> do
--            v <- new (flip Val None) empty (Constr (if b then cTrue else cFalse) Nil)
--            pure (spy "Processing boolean" (render <<< pretty) v)
--       )
--       ( \n -> do
--            v <- do
--               case Int.fromNumber n of
--                  Just n' -> new (flip Val None) empty (Int n')
--                  Nothing -> new (flip Val None) empty (Float n)
--            pure (spy "Processing number" (render <<< pretty) v)
--       )
--       ( \s -> do
--            v <- new (flip Val None) empty (Str s)
--            pure (spy "Processing string" (render <<< pretty) v)
--       )
--       ( \arr -> do
--            vs <- traverse fromJsonVal' arr
--            v <- arrVtoVal' (Array.toUnfoldable vs :: List (Val Vertex))
--            pure (spy "Processing array" (render <<< pretty) v)
--       )
--       ( \obj -> do
--            let kvs = FO.toUnfoldable obj :: Array (Tuple String Json) --turn obj into arr of key-value pairs
--            entries <- traverse
--               ( \(Tuple k vj) -> do -- k is key and vj is JSON value
--                    vv <- fromJsonVal' vj -- convert JSON value to Val Vertex
--                    pure (Tuple k (addr vv × vv)) -- convert Val Vertex to address and value
--               )
--               kvs -- traverse over the key-value pairs
--            let
--               d = D.fromFoldable entries -- create dictionary from key-value pairs
--               deps = Set.fromFoldable (entries <#> \(Tuple _ (β × _)) -> β) --map each entry to its address β, then turn list into set. β gets first element of pair
--            v <- new (flip Val None) deps (Dictionary (DictRep d)) -- deps is set of addresses the value depends on
--            pure (spy "Processing object" (render <<< pretty) v)
--       )

-- caseNull = do
--   v <- new (flip Val None) empty (Constr cNone Nil)
--   pure (spy "Processing null" (render <<< pretty) v)

-- caseNull :: forall m. MonadWithGraphAlloc m => Unit -> m (Val Vertex)
-- caseNull _ = do
--          error ("Error, Null JSON value cannot be converted to Val Vertex")

-- caseBool :: forall m. MonadWithGraphAlloc m => Boolean -> m (Val Vertex)
-- caseBool b =
--    new (flip Val None) empty (Constr (if b then cTrue else cFalse) Nil)

-- caseNumber :: forall m. MonadWithGraphAlloc m => Number -> m (Val Vertex)
-- caseNumber n = do
--    v <-
--       case Int.fromNumber n of
--          Just n' -> new (flip Val None) empty (Int n')
--          Nothing -> new (flip Val None) empty (Float n)
--    pure (spy "Processing number" (render <<< pretty) v)

-- caseString :: forall m. MonadWithGraphAlloc m => String -> m (Val Vertex)
-- caseString s = do
--    v <- new (flip Val None) empty (Str s)
--    pure (spy "Processing string" (render <<< pretty) v)

-- caseArray :: forall m. MonadWithGraphAlloc m => Array Json -> m (Val Vertex)
-- caseArray arr = do
--    vs <- traverse fromJsonVal' arr
--    v <- arrVtoVal' (Array.toUnfoldable vs :: List (Val Vertex))
--    pure (spy "Processing array" (render <<< pretty) v)

-- caseObject :: forall m. MonadWithGraphAlloc m => FO.Object Json -> m (Val Vertex)
-- caseObject obj = do
--    let kvs = FO.toUnfoldable obj :: Array (Tuple String Json) -- turn obj into arr of key-value pairs
--    entries <- traverse
--       ( \(Tuple k vj) -> do -- k is key and vj is JSON value
--            vv <- fromJsonVal' vj -- convert JSON value to Val Vertex
--            pure (Tuple k (addr vv × vv)) -- convert Val Vertex to address and value
--       )
--       kvs -- traverse over the key-value pairs
--    let
--       d = D.fromFoldable entries -- create dictionary from key-value pairs
--       deps = Set.fromFoldable (entries <#> \(Tuple _ (β × _)) -> β) -- map each entry to its address β, then turn list into set. β gets first element of pair
--    v <- new (flip Val None) deps (Dictionary (DictRep d)) -- deps is set of addresses the value depends on
--    pure (spy "Processing object" (render <<< pretty) v)

dims :: ForeignOp
dims =
   ForeignOp ("dims" × ForeignOp' { arity: 1, op: op })
   where
   op :: Op
   op (Val α _ (Matrix (MatrixRep (_ × MatrixDim (i × β1) × MatrixDim (j × β2)))) : Nil) = do
      v1 <- new (flip Val None) (singleton β1) $ Int i
      v2 <- new (flip Val None) (singleton β2) $ Int j
      let v = Constr cPair (v1 : v2 : Nil)
      new (flip Val None) (singleton α) v
   op _ = throw "Matrix expected"

matrixLookup :: ForeignOp
matrixLookup =
   ForeignOp ("!" × ForeignOp' { arity: 2, op: op })
   where
   op :: Op
   op (Val _ _ (Matrix r) : Val _ _ (Constr c (Val _ _ (Int i) : Val _ _ (Int j) : Nil)) : Nil) | c == cPair =
      pure $ matrixGet i j r
   op _ = throw "Matrix and pair of integers expected"

matrixUpdate :: ForeignOp
matrixUpdate =
   ForeignOp ("matrixUpdate" × ForeignOp' { arity: 3, op: op })
   where
   op :: Op
   op (Val α _ (Matrix r) : Val _ _ (Constr c (Val _ _ (Int i) : Val _ _ (Int j) : Nil)) : v : Nil)
      | c == cPair = new (flip Val None) (singleton α) (Matrix (matrixPut i j (const v) r))
   op _ = throw "Matrix, pair of integers and value expected"

dict_difference :: ForeignOp
dict_difference =
   ForeignOp ("dict_difference" × ForeignOp' { arity: 2, op: op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) =
      new (flip Val None) (singleton α # Set.insert β) (Dictionary (DictRep (d \\ d')))
   op _ = throw "Dictionaries expected."

dict_disjointUnion :: ForeignOp
dict_disjointUnion =
   ForeignOp ("dict_disjointUnion" × ForeignOp' { arity: 2, op: op })
   where
   op :: Op
   op (Val α _ (Dictionary (DictRep d)) : Val β _ (Dictionary (DictRep d')) : Nil) = do
      new (flip Val None) (singleton α # Set.insert β) (Dictionary (DictRep (disjointUnion d d')))
   op _ = throw "Dictionaries expected"

dict_foldl :: ForeignOp
dict_foldl =
   ForeignOp ("dict_foldl" × ForeignOp' { arity: 3, op: op })
   where
   op :: Op
   op (v : u : Val _ _ (Dictionary (DictRep d)) : Nil) =
      foldM (\u1 (_ × u2) -> G.apply v u1 >>= flip G.apply u2) u d
   op _ = throw "Function, value and dictionary expected"

dict_get :: ForeignOp
dict_get =
   ForeignOp ("dict_get" × ForeignOp' { arity: 2, op: op })
   where
   op :: Op
   op (Val _ _ (Str s) : Val _ _ (Dictionary (DictRep d)) : Nil) =
      snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
   op _ = throw "String and dictionary expected"

dict_intersectionWith :: ForeignOp
dict_intersectionWith =
   ForeignOp ("dict_intersectionWith" × ForeignOp' { arity: 3, op: op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d1)) : Val α' _ (Dictionary (DictRep d2)) : Nil) = do
      v' <- Dictionary <$> (DictRep <$> sequence (intersectionWith apply' d1 d2))
      new (flip Val None) (singleton α # Set.insert α') v'
      where
      apply' (β × u) (β' × u') = do
         v''@(Val _ _ key) <- G.apply v u >>= flip G.apply u'
         Val β'' _ _ <- new (flip Val None) (singleton β # Set.insert β') key
         pure (β'' × v'')
   op _ = throw "Function and two dictionaries expected"

dict_map :: ForeignOp
dict_map =
   ForeignOp ("dict_map" × ForeignOp' { arity: 2, op: op })
   where
   op :: Op
   op (v : Val α _ (Dictionary (DictRep d)) : Nil) = do
      d' <- traverse (\(β × u) -> (β × _) <$> G.apply v u) d
      new (flip Val None) (singleton α) (Dictionary (DictRep d'))
   op _ = throw "Function and dictionary expected"

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
