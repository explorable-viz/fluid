module Pretty.Constants where

import Prelude ((<>))

import Pretty.Doc (Doc, text)

_if :: Doc
_if = text "if"

_else :: Doc
_else = text "else"

_def :: Doc
_def = text "def"

_lambda :: Doc
_lambda = text "lambda"

_match :: Doc
_match = text "match"

_case :: Doc
_case = text "case"

_for :: Doc
_for = text "for"

_in :: Doc
_in = text "in"

_colon :: Doc
_colon = text ":"

_comma :: Doc
_comma = text ","

_empty :: Doc
_empty = text "[" <> text "]"

_lbrace :: Doc
_lbrace = text "{"

_rbrace :: Doc
_rbrace = text "}"

_lbracket :: Doc
_lbracket = text "["

_rbracket :: Doc
_rbracket = text "]"

_larray :: Doc
_larray = text "[|"

_rarray :: Doc
_rarray = text "|]"

_lparen :: Doc
_lparen = text "("

_rparen :: Doc
_rparen = text ")"

_quote :: Doc
_quote = text "\""

_asterisk :: Doc
_asterisk = text "*"

_ellipsis :: Doc
_ellipsis = text ".."
