module Temp.Pretty.Constants where

import Prelude

import Data.List (List, range)
import Temp.Pretty.Doc (Doc, text)

_x :: Doc
_x = text "x"

xs :: Int -> List Doc
xs n = map (\i -> _x <> text (show i)) (range 0 (n - 1))

_if :: Doc
_if = text "if"

_else :: Doc
_else = text "else"

_lambda :: Doc
_lambda = text "lambda"

_return :: Doc
_return = text "return"

_def :: Doc
_def = text "def"

_match :: Doc
_match = text "match"

_case :: Doc
_case = text "case"

_equal :: Doc
_equal = text "="

_colon :: Doc
_colon = text ":"

_comma :: Doc
_comma = text ","

_empty :: Doc
_empty = text "[]"

_lbracket :: Doc
_lbracket = text "["

_rbracket :: Doc
_rbracket = text "]"

_lparen :: Doc
_lparen = text "("

_rparen :: Doc
_rparen = text ")"

_quote :: Doc
_quote = text "\""

_star :: Doc
_star = text "*"
