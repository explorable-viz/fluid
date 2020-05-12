module Primitive where

import Prelude
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))


data BinaryOp = BinaryOp {
   name :: String,
   op :: Int -> Int -> Int,
   prec :: Int -- 0 to 9, similar to Haskell 98
}

opName :: BinaryOp -> String
opName (BinaryOp { name }) = name

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp { name: op }) (BinaryOp { name: op' }) = op == op'

makeBinary :: String -> (Int -> Int -> Int) -> Int -> Tuple String BinaryOp
makeBinary name op prec = Tuple name $ BinaryOp { name, op, prec }

binaryOps :: Map String BinaryOp
binaryOps = fromFoldable [
   makeBinary "*" (+) 7,
   makeBinary "+" (+) 6,
   makeBinary "-" (-) 6
]
