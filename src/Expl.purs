module Expl where

import Data.List (List)
import Bindings (Var)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Util (type (×))
import Val (Env, Primitive, Val)

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
   App (Expl a × Env a × RecDefs a × Elim a) (Expl a) (Match a) (Expl a) |
   AppPrim (Expl a × Primitive) (ExplVal a) |
   AppConstr (Expl a × Ctr × List (Val a)) (ExplVal a) |
   BinaryApp (ExplVal a) (Var × Val a) (ExplVal a) |
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   MatchConstr (Ctr × List (Match a))
