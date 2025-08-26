module Temp.Parse.Constants where

import Parsing.Expr (Operator)

keyword
   :: { case :: String
      , def :: String
      , else :: String
      , for :: String
      , if :: String
      , in :: String
      , match :: String
      }
keyword =
   { if: "if"
   , else: "else"
   , def: "def"
   , match: "match"
   , case: "case"
   , for: "for"
   , in: "in"
   }

symbol
   :: { colon :: String
      , comma :: String
      , ellipsis :: String
      , lBrace :: String
      , lBracket :: String
      , lParen :: String
      , quote :: String
      , rBrace :: String
      , rBracket :: String
      , rParen :: String
      , star :: String
      }
symbol =
   { lBrace: "{"
   , rBrace: "}"
   , lBracket: "["
   , rBracket: "]"
   , lParen: "("
   , rParen: ")"
   , star: "*"
   , colon: ":"
   , comma: ","
   , quote: "\""
   , ellipsis: ".."
   }

ops :: Array Char
ops =
   [ ':'
   , '!'
   , '#'
   , '$'
   , '%'
   , '&'
   , '*'
   , '+'
   , '.'
   , '/'
   , '<'
   , '='
   , '>'
   , '?'
   , '@'
   , '\\'
   , '^'
   , '|'
   , '-'
   , '~'
   ]
