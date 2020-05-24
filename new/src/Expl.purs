module Expl where

import Data.Tuple (Tuple)
import Bindings (Var)
import Expr (Defs, Elim, Expr)
import Val (Env)

data Expl =
     Var Var
   | Int Int
   | Pair Expl Expl
   | Nil
   | Cons Expl Expl
   | Op Var
   | App Expl Expl (Match Expr) Expl
   | AppOp Expl Expl
   | Match Expl (Match Expr) Expl
   | BinaryApp Expl Var Expl
   | Let Var Expl Expl
   | Letrec Defs Expl
   | Fun Env (Elim Expr)
   | True
   | False

data Match k =
     MatchVar Var
   | MatchTrue k
   | MatchFalse k
   | MatchPair (Match (Elim k)) (Match k)
   | MatchNil (Elim (Elim k))
   | MatchCons { nil :: k, cons :: Tuple (Match (Elim k)) (Match k) }
