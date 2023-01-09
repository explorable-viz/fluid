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
import Val (Array2, PrimOp, Val)

data Trace
   = Var Var
   | Op Var
   | Const
   | Record (Dict Trace)
   | Dictionary (List (String × Trace × Trace)) (Dict (Raw Val))
   | Constr Ctr (List Trace)
   | Matrix (Array2 Trace) (Var × Var) (Int × Int) Trace
   | Project Trace Var
   | App Trace Trace AppTrace
   | Let VarDef Trace
   | LetRec (Raw RecDefs) Trace

data AppTrace
   = AppClosure (Set Var) Match Trace
   | AppPrimitive (PrimOp × List (Raw Val)) (Raw Val) -- original arguments
   | AppConstr (Ctr × Int) -- number of original arguments (unsaturated)

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
