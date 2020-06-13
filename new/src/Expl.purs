module Expl where

import Prelude
import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Elim (Elim)
import Expr (Cont, RecDefs, Expr)
import Util (type (×))

data Def = Def (Match Unit) Expl

data Expl =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expl) |
   True | False |
   Pair Expl Expl |
   Nil | Cons Expl Expl |
   Lambda (Elim Expr) |
   App Expl Expl (Match Expr) Expl |
   AppOp Expl Expl |
   BinaryApp Expl Var Expl |
   MatchAs Expl Match2 Expl |
   Let Def Expl |
   LetRec RecDefs Expl

data Match k =
   MatchVar Var |
   MatchTrue k |
   MatchFalse k |
   MatchPair (Match (Elim k)) (Match k) |
   MatchNil (Elim (Elim k)) |
   MatchCons { nil :: k, cons :: Match (Elim k) × Match k }

data Match2 =
   MatchVar2 Var |
   MatchConstr (Ctr × List Match2) (Map Ctr Cont)
