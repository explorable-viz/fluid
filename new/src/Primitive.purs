module Primitive where

import Prelude
import Bindings (Var, ε, (:+:), (↦))
import Selected (Selected, (∧))
import Val (BinaryOp(..), BinaryOp2(..), Env, Op2(..), RawVal(..), Val(..), val)
import Data.Map (Map, fromFoldable)
import Data.Newtype (unwrap)
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

applyOp :: Op2 -> Val -> Val -> Val
applyOp (IntIntInt f) (Val _ u1) (Val _ u2) = val $ Int $ f (toInt u1) (toInt u2)
applyOp (IntIntBool f) (Val _ u1) (Val _ u2) = val $ if f (toInt u1) (toInt u2) then True else False

apply :: BinaryOp2 -> Val -> Val -> Val
apply = unwrap >>> _.fun >>> applyOp

class LiftVal a where
   liftVal :: (Int -> Int -> a) -> Val -> Val -> Val
   liftVal_fwd :: (Int -> Int -> a) -> Selected -> Val -> Val -> Val

instance intLiftVal :: LiftVal Int where
   liftVal f (Val _ u1) (Val _ u2) = val $ Int $ f (toInt u1) (toInt u2)
   liftVal_fwd f α (Val α1 u1) (Val α2 u2) = Val (α ∧ α1 ∧ α2) $ Int $ f (toInt u1) (toInt u2)

instance boolLiftVal :: LiftVal Boolean where
   liftVal f (Val _ u1) (Val _ u2) = val $ if f (toInt u1) (toInt u2) then True else False
   liftVal_fwd f α (Val α1 u1) (Val α2 u2) = Val (α ∧ α1 ∧ α2) $ if f (toInt u1) (toInt u2) then True else False

primitive :: String -> (Int -> Int -> Int) -> Val
primitive name fun =
   val $ Op $ BinaryOp { name, fun }

primitives :: Env
primitives = ε :+:
   "+" ↦ primitive "prim-plus" (+) :+:
   "-" ↦ primitive "prim-minus" (-) :+:
   "*" ↦ primitive "prim-times" (*) :+:
   "div" ↦ primitive "prim-div" div
