module Primitive where

import Prelude
import Bindings (Var, ε, (:+:), (↦))
import Selected (Selected, (∧))
import Val (BinaryOp(..), Env, Op(..), RawVal(..), Val(..), val)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Util (error)

data OpName = OpName {
   op :: Var, -- name in user land
   prec :: Int -- 0 to 9, similar to Haskell 98
}

opPrec :: OpName -> Int
opPrec (OpName { prec }) = prec

makeOpName :: String -> Int -> Tuple String OpName
makeOpName op prec = Tuple op $ OpName { op, prec }

-- Syntactic information only. No guarantee that any of these will be defined.
opNames :: Map String OpName
opNames = fromFoldable [
   makeOpName "*" 7,
   makeOpName "+" 6,
   makeOpName "-" 6,
   makeOpName "==" 4,
   makeOpName "/=" 4,
   makeOpName "<" 4,
   makeOpName ">" 4,
   makeOpName "<=" 4,
   makeOpName ">=" 4
]

-- Enforce argument type requirements.
toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"

class Blah a where
   to :: RawVal -> a
   from :: a -> RawVal

instance intBlah :: Blah Int where
   to (Int n) = n
   to _ = error "Integer expected"
   from = Int

applyOp :: Op -> Val -> Val -> Val
applyOp (IntIntInt f) (Val _ u1) (Val _ u2) =
   val $ Int $ f (toInt u1) (toInt u2)
applyOp (IntIntBool f) (Val _ u1) (Val _ u2) =
   val $ if f (toInt u1) (toInt u2) then True else False

applyOp_fwd :: Op -> Selected -> Val -> Val -> Val
applyOp_fwd (IntIntInt f) α (Val α1 u1) (Val α2 u2) =
   Val (α ∧ α1 ∧ α2) $ Int $ f (toInt u1) (toInt u2)
applyOp_fwd (IntIntBool f) α (Val α1 u1) (Val α2 u2) =
   Val (α ∧ α1 ∧ α2) $ if f (toInt u1) (toInt u2) then True else False

apply :: BinaryOp -> Val -> Val -> Val
apply (BinaryOp _ fun) = applyOp fun

apply_fwd :: BinaryOp -> Selected -> Val -> Val -> Val
apply_fwd (BinaryOp _ fun) = applyOp_fwd fun

prim_IntIntInt :: String -> (Int -> Int -> Int) -> Val
prim_IntIntInt name f = val $ Op $ BinaryOp name (IntIntInt f)

primitives :: Env
primitives = ε :+:
   "+" ↦ prim_IntIntInt "prim-plus" (+) :+:
   "-" ↦ prim_IntIntInt "prim-minus" (-) :+:
   "*" ↦ prim_IntIntInt "prim-times" (*) :+:
   "div" ↦ prim_IntIntInt "prim-div" div
