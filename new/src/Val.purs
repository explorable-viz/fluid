module Val where

import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (Selected(..))

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- Operators have "internal" names for printing, unrelated to any identifiers they happen to be bound to.
data UnaryOp =
   UnaryOp String Unary |
   PartialApp BinaryOp Val

data BinaryOp = BinaryOp String Binary

data RawVal =
   True | False |
   Int Int |
   Str String |
   Closure Env RecDefs (Elim Expr) |
   Binary BinaryOp |
   Unary UnaryOp |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val
