module Primitive where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable)
import Debug.Trace (trace)
import Math (log)
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), (:+:), (↦))
import DataType (cTrue, cFalse, Ctr(..))
import Lattice (𝔹, (∧))
import Expr (Expr(Expr), RawExpr(..), Var, expr)
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
   opDef "*"   7 AssocLeft,
   opDef "/"   7 AssocLeft,
   opDef "+"   6 AssocLeft,
   opDef "-"   6 AssocLeft,
   opDef ":"   6 AssocRight,
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

instance exprToList :: ToList (Expr Boolean) where
   toList (Expr a (Constr (Ctr ":") (e:es:Nil))) = (e:toList es)
   toList (Expr a (Constr (Ctr "Nil") Nil)) = Nil
   toList e = error "expected list expression"

instance exprFromList :: FromList (Expr Boolean) where
   fromList (x:xs) = expr $ (Constr (Ctr ":") (x:fromList xs:Nil))
   fromList Nil    = expr $ Constr (Ctr "Nil") Nil

-- Enforce primitive argument types.
class To a where
   to :: Val 𝔹 -> a

class From a where
   from :: a -> Val 𝔹

instance toInt :: To Int where
   to (Val _ (V.Int n)) = n
   to _                 = error "Integer expected"

instance fromInt :: From Int where
   from = V.Int >>> val

instance toNumber :: To Number where
   to (Val _ (V.Float n))  = n
   to _                    = error "Float expected"

instance toString :: To String where
   to (Val _ (V.Str str))  = str
   to _                    = error "String expected"

instance fromNumber :: From Number where
   from = V.Float >>> val

instance toIntOrNumber :: To (Either Int Number) where
   to (Val _ (V.Int n))    = Left n
   to (Val _ (V.Float n))  = Right n
   to _                    = error "Integer or float expected"

instance fromIntOrNumber :: From (Either Int Number) where
   from (Left n)   = val $ V.Int n
   from (Right n)  = val $ V.Float n

true_ :: Val 𝔹
true_ = val $ V.Constr cTrue Nil

false_ :: Val 𝔹
false_ = val $ V.Constr cFalse Nil

instance fromVal :: From (Val Boolean) where
   from = identity

instance fromBoolean :: From Boolean where
   from b = if b then true_ else false_

instance fromString :: From String where
   from = V.Str >>> val

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

apply :: Primitive -> Val 𝔹 -> Val 𝔹
apply (ValOp op)           = op
apply (IntOp op)           = op <<< to
apply (NumberOp op)        = op <<< to
apply (IntOrNumberOp op)   = op <<< to
apply (StringOp op)        = op <<< to

apply_fwd :: Primitive -> 𝔹 -> Val 𝔹 -> Val 𝔹
apply_fwd _ _ Hole         = Hole
apply_fwd φ α v@(Val α' _) = case apply φ v of
   Hole     -> error absurd
   Val _ u  -> Val (α ∧ α') u

primitives :: Env 𝔹
primitives = foldl (:+:) Empty [
   -- need to instantiate the corresponding PureScript primitive at a concrete type
   "+"         ↦ from   ((+) `union2` (+)),
   "-"         ↦ from   ((-) `union2` (-)),
   "*"         ↦ from   ((*) `union2` (*)),
   "/"         ↦ from   ((/)  :: Number -> Number -> Number),
   "=="        ↦ from   ((==) :: Int -> Int -> Boolean),
   "/="        ↦ from   ((/=) :: Int -> Int -> Boolean),
   "<"         ↦ from   ((<)  :: Int -> Int -> Boolean),
   ">"         ↦ from   ((>)  :: Int -> Int -> Boolean),
   "<="        ↦ from   ((<=) :: Int -> Int -> Boolean),
   ">="        ↦ from   ((>=) :: Int -> Int -> Boolean),
   "ceiling"   ↦ from   ceil,
   "debugLog"  ↦ from   ((\x -> trace x \_ -> x) :: Endo (Val 𝔹)),
   "div"       ↦ from   (div  :: Int -> Int -> Int),
   "error"     ↦ from   (error :: String -> Boolean),
   "floor"     ↦ from   floor,
   "log"       ↦ from   log,
   "numToStr"  ↦ from   (show `union` show)
]

union :: forall a . (Int -> a) -> (Number -> a) -> Int + Number -> a
union f _ (Left x) = f x
union _ f (Right x) = f x

union2 :: (Int -> Int -> Int) -> (Number -> Number -> Number) -> Int + Number -> Int + Number -> Int + Number
union2 f _ (Left x) (Left y) = Left $ f x y
union2 _ f (Left x) (Right y) = Right $ f (toNumber x) y
union2 _ f (Right x) (Left y) = Right $ f x (toNumber y)
union2 _ f (Right x) (Right y) = Right $ f x y
