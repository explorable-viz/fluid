module Pretty.Constants where

import Prelude ((<>))

import Parse.Constants (keyword, symbol)
import Pretty.Doc (Doc, text)

_if :: Doc
_if = text keyword.if

_else :: Doc
_else = text keyword.else

_def :: Doc
_def = text keyword.def

_lambda :: Doc
_lambda = text keyword.lambda

_match :: Doc
_match = text keyword.match

_case :: Doc
_case = text keyword.case

_for :: Doc
_for = text keyword.for

_in :: Doc
_in = text keyword.in

_colon :: Doc
_colon = text symbol.colon

_comma :: Doc
_comma = text symbol.comma

_empty :: Doc
_empty = text symbol.lBracket <> text symbol.rBracket

_lbrace :: Doc
_lbrace = text symbol.lBrace

_rbrace :: Doc
_rbrace = text symbol.rBrace

_lbracket :: Doc
_lbracket = text symbol.lBracket

_rbracket :: Doc
_rbracket = text symbol.rBracket

_larray :: Doc
_larray = text symbol.lArray

_rarray :: Doc
_rarray = text symbol.rArray

_lparen :: Doc
_lparen = text symbol.lParen

_rparen :: Doc
_rparen = text symbol.rParen

_quote :: Doc
_quote = text symbol.quote

_asterisk :: Doc
_asterisk = text symbol.star

_ellipsis :: Doc
_ellipsis = text symbol.ellipsis
