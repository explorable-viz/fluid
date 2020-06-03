module Primitive where

import Prelude
import Bindings (Var, ε, (:+:), (↦))
import Selected (Selected, (∧))
import Val (BinaryOp(..), Env, Val(..), RawVal(..), toInt, val)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))

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

liftVal :: (Int -> Int -> Int) -> Val -> Val -> Val
liftVal f (Val _ u1) (Val _ u2) = val $ Int $ f (toInt u1) (toInt u2)

liftVal_fwd :: (Int -> Int -> Int) -> Selected -> Val -> Val -> Val
liftVal_fwd f α (Val α1 u1) (Val α2 u2) = Val (α ∧ α1 ∧ α2) $ Int $ f (toInt u1) (toInt u2)

primitive :: String -> (Int -> Int -> Int) -> Val
primitive name fun =
   val $ Op $ BinaryOp { name, fun }

primitives :: Env
primitives = ε :+:
   "+" ↦ primitive "prim-plus" (+) :+:
   "-" ↦ primitive "prim-minus" (-) :+:
   "*" ↦ primitive "prim-times" (*) :+:
   "div" ↦ primitive "prim-div" div
