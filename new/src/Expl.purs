module Expl where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont, Elim, RecDefs)
import Util (type (×))

data Def = Def Match Expl

data Expl =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expl) |
   True | False |
   Pair Expl Expl |
   Nil | Cons Expl Expl |
   Lambda Elim |
   App Expl Expl Match Expl |
   AppOp Expl Expl |
   BinaryApp Expl Var Expl |
   MatchAs Expl Match Expl |
   Let Def Expl |
   LetRec RecDefs Expl

data Match =
   MatchVar Var |
   MatchConstr (Ctr × List Match) (Map Ctr Cont)