module Val where

import Prelude
import Data.Function (on)
import Data.Newtype (class Newtype, un)
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (Selected(..))
import Util (error)

-- Operators have "internal" names used to provide an Eq instance; unrelated to syntactic operator name.
data BinaryOp = BinaryOp {
   name :: String,
   fun :: Int -> Int -> Int
}

data Op2 =
   IntIntInt String (Int -> Int -> Int) |
   IntIntBool String (Int -> Int -> Boolean)

newtype BinaryOp2 = BinaryOp2 {
   name :: String,
   fun :: Op2
}

derive instance newtypeBinaryOp2 :: Newtype BinaryOp2 _

instance eqBinaryOp2 :: Eq BinaryOp2 where
   eq = eq `on` (un BinaryOp2 >>> _.name)

opFun :: BinaryOp -> Int -> Int -> Int
opFun (BinaryOp { fun }) = fun

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp { name: op }) (BinaryOp { name: op' }) = op == op'

data RawVal =
   True | False |
   Int Int |
   Closure Env RecDefs (Elim Expr) |
   Op BinaryOp |
   PartialApp BinaryOp Val |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val

toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"
