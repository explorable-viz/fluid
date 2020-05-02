module Expl where

import Prelude
import Bindings (Var)
import Expr (Elim)
import Val (Env)

data Match = MatchVar Var
           | MatchTrue
           | MatchFalse
           | MatchPair Var Var
           | MatchNil
           | MatchCons Var Var

derive instance eqMatch :: Eq Match

data Expl =  Bottom
           | Var Var
           | Int Int
           | Pair Expl Expl
           | Nil
           | Cons Expl Expl
           | App Expl Expl Match Expl
           | Match Expl Match Expl
           | Add Expl Expl
           | Let Var Expl Expl
           | Letrec Var Expl Expl
           | Fun Env Elim
           | True
           | False

derive instance eqExpl :: Eq Expl
