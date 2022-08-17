module Expl where

import Prelude
import Data.List (List(..), singleton)
import Bindings (Bindings, Var, val)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Util (type (×))
import Util.SnocList (SnocList, toList, reverse)
import Val (Array2, Env2, PrimOp, Val)

data VarDef a = VarDef (Match a) (Expl a)

-- We record "raw" (unannotated) values in some cases; represent as values annotated with false.
data Expl a =
   Var (Env2 a) Var |
   Op (Env2 a) Var |
   Int (Env2 a) Int |
   Float (Env2 a) Number |
   Str (Env2 a) String |
   Record (Env2 a) (Bindings (Expl a)) |
   Constr (Env2 a) Ctr (List (Expl a)) |
   Matrix (Array2 (Expl a)) (Var × Var) (Int × Int) (Expl a) |
   Lambda (Env2 a) (Elim a) |
   RecordLookup (Expl a) (SnocList Var) Var |
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

-- TODO: unify with bv
vars :: forall a . Match a -> List Var
vars (MatchVar x)          = singleton x
vars (MatchVarAnon _)      = Nil
vars (MatchConstr _ ws _)  = ws <#> vars # join
vars (MatchRecord xws)     = ws <#> vars # join
   where ws = xws # (reverse >>> toList) <#> val
