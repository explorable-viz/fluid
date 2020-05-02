module Val where

import Data.Eq (class Eq)
import Bindings
import Expr (Elim)

type Env = Bindings Val

data Val = Bot
         | True | TrueSel
         | False | FalseSel
         | Int Int | IntSel Int
         | Closure Env String Elim
         | Pair Val Val | PairSel Val Val
         | Nil | NilSel
         | Cons Val Val | ConsSel Val Val
         | Failure String

derive instance eqVal :: Eq Val
