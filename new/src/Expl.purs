module Expl where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont, Elim, RecDefs)
import Util (type (×))

data VarDef = VarDef Match Expl

data Expl =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Ctr Ctr |
   Constr Ctr (List Expl) |
   Lambda Elim |
   App Expl Expl Match Expl |
   AppOp Expl Expl |
   BinaryApp Expl Var Expl |
   MatchAs Expl Match Expl |
   Let VarDef Expl |
   LetRec RecDefs Expl

data Match =
   MatchVar Var |
   MatchConstr (Ctr × List Match) (Map Ctr Cont)