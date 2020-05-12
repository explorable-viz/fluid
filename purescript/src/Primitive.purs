module Primitive where

import Prelude ((+))
import Data.Eq (class Eq, (==))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))


data BinaryOp = BinaryOp {
   name :: String,
   op :: Int -> Int -> Int
}

opName :: BinaryOp -> String
opName (BinaryOp { name }) = name

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp { name: op }) (BinaryOp { name: op' }) = op == op'

add :: BinaryOp
add = BinaryOp { name: "+", op: (+) }

binaryOps :: Map String BinaryOp
binaryOps = fromFoldable [
   Tuple (opName add) add
]
