module Val where

import Data.Eq (class Eq, (==))
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (Selected(..))
import Util (error)

data BinaryOp = BinaryOp {
   name :: String, -- internal name used to provide an Eq instance; unrelated to operator name
   fun :: Int -> Int -> Int
}

opFun :: BinaryOp -> Int -> Int -> Int
opFun (BinaryOp { fun }) = fun

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp { name: op }) (BinaryOp { name: op' }) = op == op'

data RawVal =
     True | False
   | Int Int
   | Closure Env RecDefs (Elim Expr)
   | Op BinaryOp
   | PartialApp BinaryOp Val
   | Pair Val Val
   | Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val

toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"
