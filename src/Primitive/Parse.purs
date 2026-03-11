module Primitive.Parse where

import Prelude

import Bind (Var)
import Data.Array (length, mapWithIndex)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Parsing.Expr (Assoc(..))
import Util ((×))

data OpDef =
   Infix InfixParser Var Assoc

data InfixParser = Symbol | Ident | ConsOp | Custom

name :: OpDef -> Var
name (Infix _ n _) = n

-- Aim to match Python operator precedence and associativty as defined in:
-- https://docs.python.org/3/reference/expressions.html#operator-precedence
opDefs :: Array (Array OpDef)
opDefs =
   [ [ Infix Symbol "!" AssocLeft ]
   , [ Infix Symbol "**" AssocRight ]
   , [ Infix Symbol "*" AssocLeft
     , Infix Symbol "/" AssocLeft
     , Infix Symbol "//" AssocLeft
     , Infix Symbol "%" AssocLeft
     ]
   , [ Infix Symbol "+" AssocLeft
     , Infix Symbol "-" AssocLeft
     ]
   , [ Infix ConsOp ":" AssocRight ]
   , [ Infix Symbol "++" AssocRight ]
   , [ Infix Custom "|x|" AssocLeft ]
   , [ Infix Symbol "==" AssocNone
     , Infix Symbol "/=" AssocNone
     , Infix Symbol "<" AssocLeft
     , Infix Symbol ">" AssocLeft
     , Infix Symbol "<=" AssocLeft
     , Infix Symbol ">=" AssocLeft
     ]
   , [ Infix Ident "and" AssocLeft ]
   , [ Infix Ident "or" AssocLeft ]
   ]

-- name -> prec
opPrecs :: Map String Int
opPrecs = fromFoldable do
   i × ops <- mapWithIndex (×) opDefs
   op <- ops
   pure $ name op × (length opDefs - i) -- table is high-low

getPrec :: String -> Int
getPrec op = case lookup op opPrecs of
   Just p -> p
   Nothing -> -1
