module Expl where

import Data.List (List)
import Bindings (Var)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Util (type (×))
import Val (Env, Val)

type ExplVal a = Expl a × Val a

data VarDef a = VarDef (Match a) (Expl a)

-- Easier to store environments than contexts in our setting.
data Expl a =
   Var (Env a) Var |
   Op (Env a) Var |
   Int (Env a) Int |
   Float (Env a) Number |
   Str (Env a) String |
   Constr (Env a) Ctr (List (Expl a)) |
   Matrix (Array (Array (Expl a))) (Var × Var) (Int × Int) (Expl a) |
   Lambda (Env a) (Elim a) |
   App (Expl a × RecDefs a) (Expl a) (Match a) (Expl a) |
   AppOp (ExplVal a) (ExplVal a) |
   BinaryApp (ExplVal a) (Var × Val a) (ExplVal a) |
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   MatchConstr (Ctr × List (Match a))
