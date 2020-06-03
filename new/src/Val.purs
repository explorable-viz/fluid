module Val where

import Prelude
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (Selected(..))

data Op =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- Operators have "internal" names used to provide an Eq instance; unrelated to syntactic operator name.
data BinaryOp = BinaryOp String Op

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp name _) (BinaryOp name' _) = name == name'

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
