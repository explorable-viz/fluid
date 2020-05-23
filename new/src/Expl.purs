module Expl where

import Prelude
import Data.Tuple (Tuple)
import Bindings (Var)
import Expr (Elim, Elim2)
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

data Match2 k =
     MatchVar2 Var
   | MatchTrue2 k
   | MatchFalse2 k
   | MatchPair2 (Match2 (Elim2 k)) (Match2 k)
   | MatchNil2 (Elim2 (Elim2 k))
   | MatchCons2 { nil :: k, cons :: Tuple (Match2 (Elim2 k)) (Match2 k) }

derive instance eqMatch :: Eq Match
