module Expl where

import Data.Tuple (Tuple)
import Bindings (Var)
import Expr (Defs, Elim, Expr)

data Expl =
     Var Var
   | Op Var
   | Int Int
   | True | False
   | Pair Expl Expl
   | Nil | Cons Expl Expl
   | Lambda (Elim Expr)
   | App Expl Expl (Match Expr) Expl
   | AppOp Expl Expl
   | BinaryApp Expl Var Expl
   | Match Expl (Match Expr) Expl
   | Let Var Expl Expl
   | Letrec Defs Expl

data Match k =
     MatchVar Var
   | MatchTrue k
   | MatchFalse k
   | MatchPair (Match (Elim k)) (Match k)
   | MatchNil (Elim (Elim k))
   | MatchCons { nil :: k, cons :: Tuple (Match (Elim k)) (Match k) }
