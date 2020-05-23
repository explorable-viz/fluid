module Expl where

import Prelude
import Data.Tuple (Tuple)
import Bindings (Var)
import Expr (Elim, Elim2, Expr2)
import Val (Env, Env2)

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

data Expl2 =
     Var2 Var
   | Int2 Int
   | Pair2 Expl2 Expl2
   | Nil2
   | Cons2 Expl2 Expl2
   | Op2 Var
   | App2 Expl2 Expl2 (Match2 Expr2) Expl2
   | AppOp2 Expl2 Expl2
   | Match2 Expl2 (Match2 Expr2) Expl2
   | BinaryApp2 Expl2 Var Expl2
   | Let2 Var Expl2 Expl2
   | Letrec2 Var Expl2 Expl2
   | Fun2 Env2 (Elim2 Expr2)
   | True2
   | False2

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
