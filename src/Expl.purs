module Expl where

import Data.List (List)
import Bindings (Var)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Util (type (×))
import Val2 (Env, PrimOp, Val)

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
   AppPrim (Expl a × PrimOp) (ExplVal a) |
   AppConstr (Expl a × Ctr × List (Val a)) (ExplVal a) |
   BinaryApp (ExplVal a) (Var × PrimOp) PrimOp (ExplVal a) | -- second primop is partial application of first
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

-- Constructor matches store the non-matched constructors too, because we tolerate partial eliminators
-- and need hole expansion to be be defined for those too.
data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   MatchConstr Ctr (List (Match a)) (List Ctr)
