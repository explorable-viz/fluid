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
import Bindings (Bindings(..), (:+:), (↦))
import DataType (cTrue, cFalse)
import Lattice (𝔹, (∧))
import Expr (Var)
import Util (Endo, type (×), (×), type (+), absurd, error)
import Val (Env, Primitive(..), Val(..), val)
import Val (RawVal(..)) as V

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
   to (Val _ (V.Int n)) = n
   to _                 = error "Int expected"

instance fromInt :: From Int where
   from = V.Int >>> val

instance toNumber :: To Number where
   to (Val _ (V.Float n))  = n
   to _                    = error "Float expected"

instance fromNumber :: From Number where
   from = V.Float >>> val

instance toString :: To String where
   to (Val _ (V.Str str))  = str
   to _                    = error "Str expected"

instance fromString :: From String where
   from = V.Str >>> val

instance toIntOrNumber :: To (Either Int Number) where
   to (Val _ (V.Int n))    = Left n
   to (Val _ (V.Float n))  = Right n
   to _                    = error "Int or Float expected"

instance fromIntOrNumber :: From (Either Int Number) where
   from (Left n)   = val $ V.Int n
   from (Right n)  = val $ V.Float n

instance toIntOrNumberOrString :: To (Either (Either Int Number) String) where
   to (Val _ (V.Int n))    = Left (Left n)
   to (Val _ (V.Float n))  = Left (Right n)
   to (Val _ (V.Str n))    = Right n
   to _                    = error "Int, Float or Str expected"

true_ :: Val 𝔹
true_ = val $ V.Constr cTrue Nil

false_ :: Val 𝔹
false_ = val $ V.Constr cFalse Nil

instance fromVal :: From (Val Boolean) where
   from = identity

instance fromBoolean :: From Boolean where
   from b = if b then true_ else false_

instance fromValOp :: From a => From (Val Boolean -> a) where
   from op = val $ V.Primitive $ ValOp $ op >>> from

instance fromIntOp :: From a => From (Int -> a) where
   from op = val $ V.Primitive $ IntOp $ op >>> from

instance fromNumberOp :: From a => From (Number -> a) where
   from op = val $ V.Primitive $ NumberOp $ op >>> from

instance fromIntOrNumberOp :: From a => From (Either Int Number -> a) where
   from op = val $ V.Primitive $ IntOrNumberOp $ op >>> from

instance fromStringOp :: From a => From (String -> a) where
   from op = val $ V.Primitive $ StringOp $ op >>> from

instance fromOrStringOp :: From a => From (Either (Either Int Number) String -> a) where
   from op = val $ V.Primitive $ IntOrNumberOrStringOp $ op >>> from

apply :: Primitive -> Val 𝔹 -> Val 𝔹
apply (ValOp op)                 = op
apply (IntOp op)                 = op <<< to
apply (NumberOp op)              = op <<< to
apply (IntOrNumberOp op)         = op <<< to
apply (StringOp op)              = op <<< to
apply (IntOrNumberOrStringOp op) = op <<< to

apply_fwd :: Primitive -> 𝔹 -> Val 𝔹 -> Val 𝔹
apply_fwd _ _ Hole         = Hole
apply_fwd φ α v@(Val α' _) = case apply φ v of
   Hole     -> error absurd
   Val _ u  -> Val (α ∧ α') u

primitives :: Env 𝔹
primitives = foldl (:+:) Empty [
   -- pow and log are not overloaded, but useful to document their type
   -- PureScript / isn't defined at Int -> Int -> Number, so roll our own
   "+"         ↦ from   ((+) `union2` (+)),
   "-"         ↦ from   ((-) `union2` (-)),
   "*"         ↦ from   ((*) `union2` (*)),
   "**"        ↦ from   (pow :: Number -> Number -> Number),
   "/"         ↦ from   ((\x y -> toNumber x / toNumber y)  `union2'` (/)),
   "=="        ↦ from   ((==) `union2'` (==) `unionDisj` (==)),
   "/="        ↦ from   ((/=) `union2'` (/=) `unionDisj` (==)),
   "<"         ↦ from   ((<)  `union2'` (<)  `unionDisj` (==)),
   ">"         ↦ from   ((>)  `union2'` (>)  `unionDisj` (==)),
   "<="        ↦ from   ((<=) `union2'` (<=) `unionDisj` (==)),
   ">="        ↦ from   ((>=) `union2'` (>=) `unionDisj` (==)),
   "++"        ↦ from   ((<>) :: String -> String -> String),
   "ceiling"   ↦ from   ceil,
   "debugLog"  ↦ from   debugLog,
   "div"       ↦ from   (div :: Int -> Int -> Int),
   "error"     ↦ from   (error :: String -> Boolean),
   "floor"     ↦ from   floor,
   "log"       ↦ from   (log :: Number -> Number),
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
