module Trace where

import Prelude
import Data.List (List)
import Data.Set (Set, empty, singleton, unions)
import Dict (Dict)
import Bindings (Var)
import DataType (Ctr)
import Expr (class BV, RecDefs, bv)
import Lattice (Raw)
import Util (type (×))
import Util.Pair (Pair)
import Val (Array2, PrimOp, Val)

-- We record "raw" (unannotated) values in some cases; represent as values annotated with false.
data Trace
   = Var Var
   | Op Var
   | Const
   | Record (Dict Trace)
   | Dictionary (List (Pair Trace))
   | Constr Ctr (List Trace)
   | Matrix (Array2 Trace) (Var × Var) (Int × Int) Trace
   | Project Trace Var
   | App (Trace × Set Var) Trace Match Trace
   | AppPrim (Trace × Raw PrimOp × List (Raw Val)) (Trace × Raw Val) -- record prior arguments
   | AppConstr (Trace × Ctr × Int) Trace -- record number of prior arguments
   | Let VarDef Trace
   | LetRec (Raw RecDefs) Trace

data VarDef = VarDef Match Trace

data Match
   = MatchVar Var (Raw Val)
   | MatchVarAnon (Raw Val)
   | MatchConstr Ctr (List Match)
   | MatchRecord (Dict Match)

instance BV Match where
   bv (MatchVar x _) = singleton x
   bv (MatchVarAnon _) = empty
   bv (MatchConstr _ ws) = unions (bv <$> ws)
   bv (MatchRecord xws) = unions (bv <$> xws)
