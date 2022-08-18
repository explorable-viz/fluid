module Expl where

import Prelude
import Data.List (List)
import Data.Set (empty, singleton, unions)
import Bindings (Bindings, Var, val)
import DataType (Ctr)
import Expr (class BV, Elim, RecDefs, bv)
import Util (type (×))
import Val (Array2, Env, PrimOp, Val)

data VarDef a = VarDef (Match a) (Expl a)

-- We record "raw" (unannotated) values in some cases; represent as values annotated with false.
data Expl a =
   Var (Env a) Var |
   Op (Env a) Var |
   Int (Env a) Int |
   Float (Env a) Number |
   Str (Env a) String |
   Record (Env a) (Bindings (Expl a)) |
   Constr (Env a) Ctr (List (Expl a)) |
   Matrix (Array2 (Expl a)) (Var × Var) (Int × Int) (Expl a) |
   Lambda (Env a) (Elim a) |
   Project (Expl a) (Bindings (Val a)) Var |
   App (Expl a × RecDefs a × Elim a) (Expl a) (Match a) (Expl a) |
   AppPrim (Expl a × PrimOp × List (Val a)) (Expl a × Val a) | -- record prior arguments
   AppConstr (Expl a × Ctr × Int) (Expl a) |                   -- record number of prior arguments
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   -- list of matches should be a snoc list; should also store continuations for non-taken branches
   MatchConstr Ctr (List (Match a)) (List Ctr) |
   MatchRecord (Bindings (Match a))

instance BV (Match a) where
   bv (MatchVar x)          = singleton x
   bv (MatchVarAnon _)      = empty
   bv (MatchConstr _ ws _)  = unions (bv <$> ws)
   bv (MatchRecord xws)     = unions (bv <$> val <$> xws)
