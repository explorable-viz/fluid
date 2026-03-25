module Primitive.Parse where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Parsing.Expr (Assoc(..))
import Util ((×))

data OpDef = OpDef String Fixity OpType

data OpType
   = Symbol
   | Ident
   | CustomOp
   | ConsOp
   | ProjectOp

data Fixity = Infix Assoc | Prefix | Postfix

op :: String -> Fixity -> OpType -> OpDef
op = OpDef

-- Aim to match Python operator precedence and associativty as defined in:
-- https://docs.python.org/3/reference/expressions.html#operator-precedence
opDefs :: Array (Array OpDef)
opDefs =
   -- Matrix lookup
   [ [ op "!" (Infix AssocLeft) Symbol ]
   -- Exponentiation
   , [ op "**" (Infix AssocRight) Symbol ]
   -- Multiplication, division, floor division, remainder
   , [ op "*" (Infix AssocLeft) Symbol
     , op "/" (Infix AssocLeft) Symbol
     , op "//" (Infix AssocLeft) Symbol
     , op "%" (Infix AssocLeft) Symbol
     ]
   -- Addition and subtraction
   , [ op "+" (Infix AssocLeft) Symbol
     , op "-" (Infix AssocLeft) Symbol
     ]
   -- Cons
   , [ op ":" (Infix AssocLeft) ConsOp ]
   -- String concat
   , [ op "++" (Infix AssocRight) Symbol ]
   -- Custom operators (as Python's bitwise OR)
   , [ op "|x|" (Infix AssocLeft) CustomOp ]
   -- Comparisons, including membership tests and identity tests
   , [ op "==" (Infix AssocNone) Symbol
     , op "/=" (Infix AssocNone) Symbol
     , op "<" (Infix AssocLeft) Symbol
     , op ">" (Infix AssocLeft) Symbol
     , op "<=" (Infix AssocLeft) Symbol
     , op ">=" (Infix AssocLeft) Symbol
     ]
   -- Boolean NOT
   , [ op "not" Prefix Ident ]
   -- Boolean AND
   , [ op "and" (Infix AssocLeft) Ident ]
   -- Boolean OR
   , [ op "or" (Infix AssocLeft) Ident ]
   ]

-- name -> prec
opPrecs :: Map String Int
opPrecs = fromFoldable do
   i × ops <- mapWithIndex (×) opDefs
   (OpDef name _ _) <- ops
   pure $ name × (length opDefs - i) -- table is high-low

getPrec :: String -> Int
getPrec name = case lookup name opPrecs of
   Just p -> p
   Nothing -> -1
