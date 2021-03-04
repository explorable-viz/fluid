module Primitive where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..))
import Data.Map (Map, fromFoldable)
import Debug.Trace (trace)
import Math (log, pow)
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), Var, (:+:), (↦))
import DataType (cCons, cTrue, cFalse)
import Lattice (𝔹, (∧))
import Util (Endo, type (×), (×), type (+), absurd, error)
import Val (Env, Primitive(..), Val, getα, setα)
import Val (Val(..)) as V

-- name in user land, precedence 0 to 9 (similar to Haskell 98), associativity
type OpDef = {
   op    :: Var,
   prec  :: Int,
   assoc :: Assoc
}

opDef :: Var -> Int -> Assoc -> Var × OpDef
opDef op prec assoc = op × { op, prec, assoc }

-- Syntactic information only. No guarantee that any of these will be defined.
opDefs :: Map String OpDef
opDefs = fromFoldable [
   opDef "**"  8 AssocRight,
   opDef "*"   7 AssocLeft,
   opDef "/"   7 AssocLeft,
   opDef "+"   6 AssocLeft,
   opDef "-"   6 AssocLeft,
   opDef ":"   6 AssocRight,
   opDef "++"  5 AssocRight,
   opDef "=="  4 AssocNone,
   opDef "/="  4 AssocNone,
   opDef "<"   4 AssocLeft,
   opDef ">"   4 AssocLeft,
   opDef "<="  4 AssocLeft,
   opDef ">="  4 AssocLeft
]

class ToList a where
   toList :: a -> List a

class FromList a where
   fromList :: List a -> a

-- Enforce primitive argument types.
class To a where
   to :: Val 𝔹 -> a

class From a where
   from :: a -> Val 𝔹

instance toInt :: To Int where
   to (V.Int _ n) = n
   to _                 = error "Int expected"

instance fromInt :: From Int where
   from = V.Int false

instance toNumber :: To Number where
   to (V.Float _ n)  = n
   to _              = error "Float expected"

instance fromNumber :: From Number where
   from = V.Float false

instance toString :: To String where
   to (V.Str _ str)  = str
   to _              = error "Str expected"

instance fromString :: From String where
   from = V.Str false

instance toIntOrNumber :: To (Either Int Number) where
   to (V.Int _ n)    = Left n
   to (V.Float _ n)  = Right n
   to _              = error "Int or Float expected"

instance fromIntOrNumber :: From (Either Int Number) where
   from (Left n)   = V.Int false n
   from (Right n)  = V.Float false n

instance toIntOrNumberOrString :: To (Either (Either Int Number) String) where
   to (V.Int _ n)    = Left (Left n)
   to (V.Float _ n)  = Left (Right n)
   to (V.Str _ n)    = Right n
   to _              = error "Int, Float or Str expected"

true_ :: Val 𝔹
true_ = V.Constr false cTrue Nil

false_ :: Val 𝔹
false_ = V.Constr false cFalse Nil

instance fromVal :: From (Val Boolean) where
   from = identity

instance fromBoolean :: From Boolean where
   from b = if b then true_ else false_

instance fromValOp :: From a => From (Val Boolean -> a) where
   from op = V.Primitive false (ValOp (op >>> from))

instance fromIntOp :: From a => From (Int -> a) where
   from op = V.Primitive false (IntOp (op >>> from))

instance fromNumberOp :: From a => From (Number -> a) where
   from op = V.Primitive false (NumberOp (op >>> from))

instance fromIntOrNumberOp :: From a => From (Either Int Number -> a) where
   from op = V.Primitive false (IntOrNumberOp (op >>> from))

instance fromStringOp :: From a => From (String -> a) where
   from op = V.Primitive false (StringOp (op >>> from))

instance fromOrStringOp :: From a => From (Either (Either Int Number) String -> a) where
   from op = V.Primitive false (IntOrNumberOrStringOp (op >>> from))

apply :: Primitive -> Val 𝔹 -> Val 𝔹
apply (ValOp op)                 = op
apply (IntOp op)                 = op <<< to
apply (NumberOp op)              = op <<< to
apply (IntOrNumberOp op)         = op <<< to
apply (StringOp op)              = op <<< to
apply (IntOrNumberOrStringOp op) = op <<< to

apply_fwd :: Primitive -> 𝔹 -> Val 𝔹 -> Val 𝔹
apply_fwd _ _ V.Hole = V.Hole
apply_fwd φ α v      =
   let α' = getα v in
   case apply φ v of
      V.Hole -> error absurd
      u -> setα (α ∧ α') u

primitives :: Env 𝔹
primitives = foldl (:+:) Empty [
   -- some signatures are documented for clarity
   -- PureScript's / and pow aren't defined at Int -> Int -> Number, so roll our own
   "+"         ↦ from   ((+) `union2` (+)),
   "-"         ↦ from   ((-) `union2` (-)),
   "*"         ↦ from   ((*) `union2` (*)),
   "**"        ↦ from   ((\x y -> toNumber x `pow` toNumber y) `union2'` pow),
   "/"         ↦ from   ((\x y -> toNumber x / toNumber y)  `union2'` (/)),
   "=="        ↦ from   ((==) `union2'` (==) `unionDisj` (==)),
   "/="        ↦ from   ((/=) `union2'` (/=) `unionDisj` (==)),
   "<"         ↦ from   ((<)  `union2'` (<)  `unionDisj` (==)),
   ">"         ↦ from   ((>)  `union2'` (>)  `unionDisj` (==)),
   "<="        ↦ from   ((<=) `union2'` (<=) `unionDisj` (==)),
   ">="        ↦ from   ((>=) `union2'` (>=) `unionDisj` (==)),
   "++"        ↦ from   ((<>) :: String -> String -> String),
   ":"         ↦ V.Constr false cCons Nil,
   "ceiling"   ↦ from   ceil,
   "debugLog"  ↦ from   debugLog,
   "div"       ↦ from   (div :: Int -> Int -> Int),
   "error"     ↦ from   (error :: String -> Boolean),
   "floor"     ↦ from   floor,
   "log"       ↦ from   ((toNumber >>> log) `union` log),
   "numToStr"  ↦ from   (show `union` show)
]

debugLog :: Endo (Val 𝔹)
debugLog x = trace x (const x)

-- Could improve this a bit with some type class shenanigans, but not straightforward.
union :: forall a . (Int -> a) -> (Number -> a) -> Int + Number -> a
union f _ (Left x)   = f x
union _ f (Right x)  = f x

union2 :: (Int -> Int -> Int) -> (Number -> Number -> Number) -> Int + Number -> Int + Number -> Int + Number
union2 f _ (Left x) (Left y)     = Left $ f x y
union2 _ f (Left x) (Right y)    = Right $ f (toNumber x) y
union2 _ f (Right x) (Right y)   = Right $ f x y
union2 _ f (Right x) (Left y)    = Right $ f x (toNumber y)

union2' :: forall a . (Int -> Int -> a) -> (Number -> Number -> a) -> Int + Number -> Int + Number -> a
union2' f _ (Left x) (Left y)    = f x y
union2' _ f (Left x) (Right y)   = f (toNumber x) y
union2' _ f (Right x) (Right y)  = f x y
union2' _ f (Right x) (Left y)   = f x (toNumber y)

unionDisj :: forall a b . (b -> b -> a) -> (String -> String -> a) -> b + String -> b + String -> a
unionDisj f _ (Left x) (Left y)   = f x y
unionDisj _ _ (Left _) (Right _)  = error "Non-uniform argument types"
unionDisj _ f (Right x) (Right y) = f x y
unionDisj _ _ (Right _) (Left _)  = error "Non-uniform argument types"
