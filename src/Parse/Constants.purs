module Parse.Constants where

keyword
   :: { case :: String
      , def :: String
      , lambda :: String
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
   , lambda: "lambda"
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
      , lArray :: String
      , rArray :: String
      }
symbol =
   { lBrace: "{"
   , rBrace: "}"
   , lBracket: "["
   , rBracket: "]"
   , lArray: "[|"
   , rArray: "|]"
   , lParen: "("
   , rParen: ")"
   , star: "*"
   , colon: ":"
   , comma: ","
   , quote: "\""
   , ellipsis: ".."
   }

opChars :: Array Char
opChars =
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
