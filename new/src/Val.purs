module Val where

import Prelude
import Bindings (Bindings, (:+:), (↦), ε)
import Expr (Defs, Elim, Expr)
import Primitive (BinaryOp(..))
import Selected (Selected(..), (∧))
import Util (error)

data RawVal =
     True | False
   | Int Int
   | Closure Env Defs (Elim Expr)
   | Op BinaryOp
   | PartialApp BinaryOp Val
   | Pair Val Val
   | Nil | Cons Val Val

type Val = { α :: Selected, u :: RawVal }

val :: RawVal -> Val
val u = { α: Bot, u }

toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"

toValues :: (Int -> Int -> Int) -> Val -> Val -> Val
toValues f { u } { u: u' } = val $ Int $ f (toInt u) (toInt u')

toValues_fwd :: (Int -> Int -> Int) -> Selected -> Val -> Val -> Val
toValues_fwd f α v v' = { α: α ∧ v.α ∧ v'.α, u: Int $ f (toInt v.u) (toInt v'.u) }

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
