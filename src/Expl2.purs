module Expl2 where

import Prelude
import Data.List (List(..), singleton)
import Bindings2 (Bindings, Var, val)
import DataType2 (Ctr)
import Expr2 (Elim, RecDefs)
import Util2 (type (×))
import Util.SnocList2 (SnocList, toList, reverse)
import Val2 (Array2, Env, PrimOp, Val)

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
   RecordLookup (Expl a) (SnocList Var) Var |
   App (Expl a × Env a × RecDefs a × Elim a) (Expl a) (Match a) (Expl a) |
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
