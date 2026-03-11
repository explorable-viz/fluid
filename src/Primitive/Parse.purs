module Primitive.Parse where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Parsing.Expr (Assoc(..))
import Util ((×))

data OpDef = OpDef String Op

data Op
   = Symbol Fixity
   | Ident Fixity
   | CustomOp
   | ConsOp
   | ProjectOp

data Fixity = Infix Assoc | Prefix | Postfix

op :: String -> Op -> OpDef
op = OpDef

-- Aim to match Python operator precedence and associativty as defined in:
-- https://docs.python.org/3/reference/expressions.html#operator-precedence
opDefs :: Array (Array OpDef)
opDefs =
   -- Matrix lookup
   [ [ op "!" symbol ]
   -- Exponentiation
   , [ op "**" symbolR ]
   -- Multiplication, division, floor division, remainder
   , [ op "*" symbol
     , op "/" symbol
     , op "//" symbol
     , op "%" symbol
     ]
   -- Addition and subtraction
   , [ op "+" symbol
     , op "-" symbol
     ]
   -- Cons
   , [ op ":" ConsOp ]
   -- String concat
   , [ op "++" symbolR ]
   -- Custom operators (as Python's bitwise OR)
   , [ op "|x|" CustomOp ]
   -- Comparisons, including membership tests and identity tests
   , [ op "==" symbolN
     , op "/=" symbolN
     , op "<" symbol
     , op ">" symbol
     , op "<=" symbol
     , op ">=" symbol
     ]
   -- Boolean NOT
   , [ op "not" identPrefix ]
   -- Boolean AND
   , [ op "and" ident ]
   -- Boolean OR
   , [ op "or" ident ]
   ]

   where

   symbol = Symbol (Infix AssocLeft)
   symbolR = Symbol (Infix AssocRight)
   symbolN = Symbol (Infix AssocNone)

   ident = Ident (Infix AssocLeft)
   identPrefix = Ident Prefix

-- name -> prec
opPrecs :: Map String Int
opPrecs = fromFoldable do
   i × ops <- mapWithIndex (×) opDefs
   (OpDef name _) <- ops
   pure $ name × (length opDefs - i) -- table is high-low

getPrec :: String -> Int
getPrec name = case lookup name opPrecs of
   Just p -> p
   Nothing -> -1
