module Expl2 where

import Data.List (List)
import Data.Map (Map)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont2', Elim2', RecDefs2')
import Lattice (Selected)
import Util (type (×))
import Val (Env2', Val2')

type ExplVal a = Expl' a × Val2' a

data VarDef a = VarDef (Match' a) (Expl' a)

data Expl' a =
   Var Var (Env2' a) |
   Op Var (Env2' a) |
   Int Int (Env2' a) |
   Str String (Env2' a) |
   Constr Ctr (List (Expl' a)) |
   NullConstr Ctr (Env2' a) |
   Lambda (Elim2' a) |
   App (ExplVal a) (Expl' a) (Match' a) (Expl' a) |
   AppOp (ExplVal a) (ExplVal a) |
   BinaryApp (ExplVal a) Var (ExplVal a) |
   MatchAs (Expl' a) (Match' a) (Expl' a) |
   Let (VarDef a) (Expl' a) |
   LetRec (RecDefs2' a) (Expl' a)

type Expl = Expl' Selected

data Match' a =
   MatchVar Var |
   MatchVarAnon (Val2' a) |
   MatchConstr (Ctr × List (Match' a)) (Map Ctr (Cont2' a))

type Match = Match' Selected
