module Val where

import Prelude
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (Selected(..))

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- Operators have "internal" names used to provide an Eq instance; unrelated to syntactic operator name.
data BinaryOp = BinaryOp String Binary

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp name _) (BinaryOp name' _) = name == name'

data RawVal =
   True | False |
   Int Int |
   Str String |
   Closure Env RecDefs (Elim Expr) |
   BinOp BinaryOp |
   PartialApp BinaryOp Val |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val
