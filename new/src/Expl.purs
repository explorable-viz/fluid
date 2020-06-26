module Expl where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont, Elim, RecDefs)
import Util (type (×))
import Val (Env, Val)

type ExplVal = Expl × Val

data Def = Def Match Expl

data Expl =
   Var Var Env |
   Op Var Env |
   Int Int Env |
   Str String |
   Constr Ctr (List Expl) |
   NullConstr Ctr Env |
   Lambda Elim |
   App ExplVal Expl Match Expl |
   AppOp ExplVal ExplVal |
   BinaryApp ExplVal Var ExplVal |
   MatchAs Expl Match Expl |
   Let Def Expl |
   LetRec RecDefs Expl

data Match =
   MatchVar Var |
   MatchConstr (Ctr × List Match) (Map Ctr Cont)
