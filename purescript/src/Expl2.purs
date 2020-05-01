module Expl2 where

import Prelude
import Expr2 (Elim, Env, Var)

data Match = MatchVar Var
           | MatchTrue
           | MatchFalse
           | MatchPair Var Var
           | MatchNil
           | MatchCons Var Var

derive instance eqMatch :: Eq Match

data Expl =  ExplBottom
           | ExplVar Var
           | ExplInt Int
           | ExplPair Expl Expl
           | ExplNil
           | ExplCons Expl Expl
           | ExplApp Expl Expl Match Expl
           | ExplMatch Expl Match Expl
           | ExplAdd Expl Expl
           | ExplLet Var Expl Expl
           | ExplLetrec Var Expl Expl
           | ExplFun Env Elim
           | ExplTrue
           | ExplFalse


derive instance eqExpl :: Eq Expl
