module Val where

import Data.Eq (class Eq)
import Bindings
import Expr (Elim, Selected)

type Env = Bindings Val

data RawVal =
     True | False
   | Int Int
   | Closure Env String Elim
   | Pair Val Val
   | Nil | Cons Val Val
   | Failure String

type Val = { α :: Selected, u :: RawVal }

derive instance eqRawVal :: Eq RawVal
