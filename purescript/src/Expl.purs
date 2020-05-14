module Expl where

import Prelude
import Bindings (Var)
import Expr (Elim)
import Val (Env)

data Expl =
     Var Var
   | Int Int
   | Pair Expl Expl
   | Nil
   | Cons Expl Expl
   | Op Var
   | App Expl Expl Match Expl
   | AppOp Expl Expl
   | Match Expl Match Expl
   | BinaryApp Expl Var Expl
   | Let Var Expl Expl
   | Letrec Var Expl Expl
   | Fun Env Elim
   | True
   | False

derive instance eqExpl :: Eq Expl

data Match =
     MatchVar Var
   | MatchTrue
   | MatchFalse
   | MatchPair Var Var
   | MatchNil
   | MatchCons Var Var

derive instance eqMatch :: Eq Match
