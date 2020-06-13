module Expl where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Elim (Elim)
import Expr (Cont, Elim2, RecDefs)
import Util (type (×))

data Def = Def Match2 Expl

data Expl =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expl) |
   True | False |
   Pair Expl Expl |
   Nil | Cons Expl Expl |
   Lambda Elim2 |
   App Expl Expl Match2 Expl |
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
