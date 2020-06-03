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
data UnaryOp = UnaryOp String Unary
data BinaryOp = BinaryOp String Binary

instance eqUnaryOp :: Eq UnaryOp where
   eq (UnaryOp name _) (UnaryOp name' _) = name == name'

instance eqBinaryOp :: Eq BinaryOp where
   eq (BinaryOp name _) (BinaryOp name' _) = name == name'

data RawVal =
   True | False |
   Int Int |
   Str String |
   Closure Env RecDefs (Elim Expr) |
   Binary BinaryOp |
   Unary UnaryOp |
   PartialApp BinaryOp Val |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val
