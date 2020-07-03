module Expl where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont', Elim', RecDefs')
import Lattice (Selected)
import Util (type (×))
import Val (Env', Val')

type ExplVal a = Expl' a × Val' a

data VarDef' a = VarDef (Match' a) (Expl' a)

data Expl' a =
   Var Var (Env' a) |
   Op Var (Env' a) |
   Int Int (Env' a) |
   Str String (Env' a) |
   Constr Ctr (List (Expl' a)) |
   NullConstr Ctr (Env' a) |
   Lambda (Elim' a) |
   App (ExplVal a) (Expl' a) (Match' a) (Expl' a) |
   AppOp (ExplVal a) (ExplVal a) |
   BinaryApp (ExplVal a) Var (ExplVal a) |
   MatchAs (Expl' a) (Match' a) (Expl' a) |
   Let (VarDef' a) (Expl' a) |
   LetRec (RecDefs' a) (Expl' a)

type Expl = Expl' Selected

data Match' a =
   MatchVar Var |
   MatchVarAnon (Val' a) |
   MatchConstr (Ctr × List (Match' a)) (Map Ctr (Cont' a))

type Match = Match' Selected
