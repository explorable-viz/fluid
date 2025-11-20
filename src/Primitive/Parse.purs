module Primitive.Parse where

import Prelude

import Bind (Var)
import Data.Map (Map, fromFoldable)
import Parsing.Expr (Assoc(..))
import Util ((×))

-- name in user land, precedence, associativity, type (for parser)
type OpDef =
   { op :: Var
   , prec :: Int
   , assoc :: Assoc
   , type :: OpType
   }

data OpType = BinaryOp | BinaryId | BinaryCons

opDef :: Var -> Int -> Assoc -> OpType -> OpDef
opDef op prec assoc ty = { op, prec, assoc, type: ty }

-- Aim to match Python operator precedence and associativty as defined in:
-- https://docs.python.org/3/reference/expressions.html#operator-precedence
opDefs :: Array OpDef
opDefs =
   [ opDef "!" 9 AssocLeft BinaryOp
   , opDef "**" 8 AssocRight BinaryOp
   , opDef "*" 7 AssocLeft BinaryOp
   , opDef "/" 7 AssocLeft BinaryOp
   , opDef "//" 7 AssocLeft BinaryOp
   , opDef "%" 7 AssocLeft BinaryOp
   , opDef "+" 6 AssocLeft BinaryOp
   , opDef "-" 6 AssocLeft BinaryOp
   , opDef ":" 5 AssocRight BinaryCons
   , opDef "++" 4 AssocRight BinaryOp
   , opDef "==" 3 AssocNone BinaryOp
   , opDef "/=" 3 AssocNone BinaryOp
   , opDef "<" 3 AssocLeft BinaryOp
   , opDef ">" 3 AssocLeft BinaryOp
   , opDef "<=" 3 AssocLeft BinaryOp
   , opDef ">=" 3 AssocLeft BinaryOp
   , opDef "and" 2 AssocLeft BinaryId
   , opDef "or" 1 AssocLeft BinaryId
   ]

-- for lookup by name
opMap :: Map String OpDef
opMap = fromFoldable $ map (\def -> def.op × def) opDefs
