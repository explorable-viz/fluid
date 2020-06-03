module Val where

import Prelude
import Bindings (Bindings, (:+:), (↦), ε)
import Expr (RecDefs, Elim, Expr)
import Primitive (BinaryOp(..))
import Selected (Selected(..), (∧))
import Util (error)

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

toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"

toValues :: (Int -> Int -> Int) -> Val -> Val -> Val
toValues f (Val _ u1) (Val _ u2) = val $ Int $ f (toInt u1) (toInt u2)

toValues_fwd :: (Int -> Int -> Int) -> Selected -> Val -> Val -> Val
toValues_fwd f α (Val α1 u1) (Val α2 u2) = Val (α ∧ α1 ∧ α2) $ Int $ f (toInt u1) (toInt u2)

type Env = Bindings Val

primitive :: String -> (Int -> Int -> Int) -> Val
primitive name fun =
   val $ Op $ BinaryOp { name, fun }

primitives :: Env
primitives = ε :+:
   "+" ↦ primitive "prim-plus" (+) :+:
   "-" ↦ primitive "prim-minus" (-) :+:
   "*" ↦ primitive "prim-times" (*) :+:
   "div" ↦ primitive "prim-div" div
