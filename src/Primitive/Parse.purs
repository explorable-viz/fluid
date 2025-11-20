module Primitive.Parse where

import Prelude

import Bind (Var)
import Data.Map (Map, fromFoldable)
import Parsing.Expr (Assoc(..))
import Util ((×))

data OpDef =
   Infix InfixParser Var Assoc Int

data InfixParser = Symbol | Ident | ConsOp | Custom

name :: OpDef -> Var
name (Infix _ n _ _) = n

prec :: OpDef -> Int
prec (Infix _ _ _ p) = p

-- Aim to match Python operator precedence and associativty as defined in:
-- https://docs.python.org/3/reference/expressions.html#operator-precedence
opDefs :: Array OpDef
opDefs =
   [ Infix Symbol "!" AssocLeft 9
   , Infix Symbol "**" AssocRight 8
   , Infix Symbol "*" AssocLeft 7
   , Infix Symbol "/" AssocLeft 7
   , Infix Symbol "//" AssocLeft 7
   , Infix Symbol "%" AssocLeft 7
   , Infix Symbol "+" AssocLeft 6
   , Infix Symbol "-" AssocLeft 6
   , Infix ConsOp ":" AssocRight 5
   , Infix Symbol "++" AssocRight 4
   , Infix Symbol "==" AssocNone 3
   , Infix Symbol "/=" AssocNone 3
   , Infix Symbol "<" AssocLeft 3
   , Infix Symbol ">" AssocLeft 3
   , Infix Symbol "<=" AssocLeft 3
   , Infix Symbol ">=" AssocLeft 3
   , Infix Ident "and" AssocLeft 2
   , Infix Ident "or" AssocLeft 1
   , Infix Custom "|x|" AssocLeft 0
   ]

-- for lookup by name
opMap :: Map String OpDef
opMap = fromFoldable $ map (\def -> name def × def) opDefs
