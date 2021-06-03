module Expl where

import Prelude
import Data.List (List(..), singleton)
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Bindings (Var)
import Bindings2 (Bindings2)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Util (type (×))
import Util.SnocList (toList, reverse)
import Val (Array2, Env, Env2, PrimOp, Val)

data VarDef a = VarDef (Match a) (Expl a)

-- We record values in some cases, which should be assumed to be "unannotated" (= annotated with false).
data Expl a =
   Var (Env2 a) Var |
   Op (Env a) Var |
   Int (Env a) Int |
   Float (Env a) Number |
   Str (Env a) String |
   Record (Env a) (Bindings2 (Expl a)) |
   Constr (Env a) Ctr (List (Expl a)) |
   Matrix (Array2 (Expl a)) (Var × Var) (Int × Int) (Expl a) |
   Lambda (Env a) (Elim a) |
   App (Expl a × Env a × RecDefs a × Elim a) (Expl a) (Match a) (Expl a) |
   AppPrim (Expl a × PrimOp × List (Val a)) (Expl a × Val a) | -- record prior arguments
   AppConstr (Expl a × Ctr × Int) (Expl a) |                   -- record number of prior arguments
   Let (VarDef a) (Expl a) |
   LetRec (RecDefs a) (Expl a)

-- Constructor matches store the non-matched constructors too, because we tolerate partial eliminators
-- and need hole expansion to be defined for those too.
data Match a =
   MatchVar Var |
   MatchVarAnon (Val a) |
   MatchConstr Ctr (List (Match a)) (List Ctr) |  -- list of matches should be a snoc list
   MatchRecord (Bindings2 (Match a))

vars :: forall a . Match a -> List Var
vars (MatchVar x)          = singleton x
vars (MatchVarAnon _)      = Nil
vars (MatchConstr _ ws _)  = ws <#> vars # join
vars (MatchRecord xws)     = ws <#> vars # join
   where ws = xws # reverse # toList <#> (unwrap >>> snd)
