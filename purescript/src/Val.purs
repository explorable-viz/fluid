module Val where

import Data.Eq (class Eq)
import Bindings
import Expr (Elim)
import Selected (Selected(..))

type Env = Bindings Val

data RawVal =
     True | False
   | Int Int
   | Closure Env String Elim
   | Pair Val Val
   | Nil | Cons Val Val

type Val = { α :: Selected, u :: RawVal }

val :: RawVal -> Val
val u = { α: Bot, u }

derive instance eqRawVal :: Eq RawVal
