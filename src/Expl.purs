module Expl where

import Data.List (List)
import Data.Map (Map)
import DataType (Ctr)
import Expr (Cont, RecDefs, Var)
import Util (type (×))
import Val (Env, Val)

type ExplVal a = Expl a × Val a

data VarDef a = VarDef (Match a) (Expl a)

data RawExpl a =
   Hole |
   Var Var |
   Op Var |
   Int |
   Float |
   Str  |
   Constr Ctr (List (Expl a)) |
   Matrix (Array (Array (Expl a))) |
   Lambda |
   AppHole (Expl a) |
   App (Expl a × RecDefs a) (Expl a) (Match a) (Expl a) |
   AppOp (ExplVal a) (ExplVal a) |
   BinaryApp (ExplVal a) (Var × Val a) (ExplVal a) |
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

data Expl a = Expl (Env a) (RawExpl a)

data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   MatchConstr (Ctr × List (Match a)) (Map Ctr (Cont a))
