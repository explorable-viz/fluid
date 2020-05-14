module Primitive where

import Prelude
import Bindings (Var)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))


data BinaryOp = BinaryOp {
   name :: String, -- internal name used to provide an Eq instance; unrelated to operator name
   fun :: Int -> Int -> Int
}

opFun :: BinaryOp -> Int -> Int -> Int
opFun (BinaryOp { fun }) = fun

data OpName = OpName {
   op :: Var, -- name in user land
   prec :: Int -- 0 to 9, similar to Haskell 98
}

opName :: OpName -> Var
opName (OpName { op }) = op

opPrec :: OpName -> Int
opPrec (OpName { prec }) = prec

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp { name: op }) (BinaryOp { name: op' }) = op == op'

makeOpName :: String -> Int -> Tuple String OpName
makeOpName op prec = Tuple op $ OpName { op, prec }

opNames :: Map String OpName
opNames = fromFoldable [
   makeOpName "*" 7,
   makeOpName "+" 6,
   makeOpName "-" 6
]
