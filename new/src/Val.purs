module Val where

import Prelude
import Bindings (Bindings, (:+:), (↦), ε)
import Expr (Elim, Elim2, Expr, Expr2)
import Primitive (BinaryOp(..))
import Selected (Selected(..), (∧))
import Util (error)

data RawVal =
     True | False
   | Int Int
   | Closure Env String Elim
   | Op BinaryOp
   | PartialApp BinaryOp Val
   | Pair Val Val
   | Nil | Cons Val Val

data RawVal2 =
     True2 | False2
   | Int2 Int
   | Closure2 Env2 String (Elim2 Expr2)
   | Op2 BinaryOp
   | PartialApp2 BinaryOp Val2
   | Pair2 Val2 Val2
   | Nil2 | Cons2 Val2 Val2

type Val = { α :: Selected, u :: RawVal }

type Val2 = { α :: Selected, u :: RawVal2 }

val :: RawVal -> Val
val u = { α: Bot, u }

val2 :: RawVal2 -> Val2
val2 u = { α: Bot, u }

derive instance eqRawVal :: Eq RawVal

toInt :: RawVal -> Int
toInt (Int n) = n
toInt _ = error "Integer expected"

toInt2 :: RawVal2 -> Int
toInt2 (Int2 n) = n
toInt2 _ = error "Integer expected"

toValues :: (Int -> Int -> Int) -> Val -> Val -> Val
toValues f { u } { u: u' } = val $ Int $ f (toInt u) (toInt u')

toValues2 :: (Int -> Int -> Int) -> Val2 -> Val2 -> Val2
toValues2 f { u } { u: u' } = val2 $ Int2 $ f (toInt2 u) (toInt2 u')

toValues_fwd :: (Int -> Int -> Int) -> Selected -> Val -> Val -> Val
toValues_fwd f α v v' = { α: α ∧ v.α ∧ v'.α, u: Int $ f (toInt v.u) (toInt v'.u) }

type Env = Bindings Val

type Env2 = Bindings Val2

primitive :: String -> (Int -> Int -> Int) -> Val
primitive name fun =
   val $ Op $ BinaryOp { name, fun }

primitives :: Env
primitives = ε :+:
   "+" ↦ primitive "prim-plus" (+) :+:
   "-" ↦ primitive "prim-minus" (-) :+:
   "*" ↦ primitive "prim-times" (*) :+:
   "div" ↦ primitive "prim-div" div

x :: Int -> Int
x = \n -> z a

a :: Int
a = b

y :: Int -> Int
y = \n -> x a

z :: Int -> Int
z = \n -> y a

b :: Int
b = 6
