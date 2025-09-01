module Parse.Constants where

-- Constants (should also be used by prettyprinter). Haven't found a way to avoid the type definition.
str
   :: { arrayLBracket :: String
      , arrayRBracket :: String
      , as :: String
      , atDoc :: String
      , backslash :: String
      , backtick :: String
      , bar :: String
      , colon :: String
      , colonEq :: String
      , comma :: String
      , curlylBrace :: String
      , curlyrBrace :: String
      , dictLBracket :: String
      , dictRBracket :: String
      , dollar :: String
      , dot :: String
      , ellipsis :: String
      , else_ :: String
      , equals :: String
      , fun :: String
      , if_ :: String
      , import :: String
      , in_ :: String
      , lArrow :: String
      , lBracket :: String
      , let_ :: String
      , lparenth :: String
      , match :: String
      , rArrow :: String
      , rBracket :: String
      , rparenth :: String
      , semiColon :: String
      , then_ :: String
      , triplequote :: String
      }

str =
   { arrayLBracket: "[|"
   , arrayRBracket: "|]"
   , as: "as"
   , atDoc: "@doc"
   , backslash: "\\"
   , backtick: "`"
   , bar: "|"
   , colon: ":"
   , colonEq: ":="
   , comma: ","
   , curlylBrace: "{"
   , curlyrBrace: "}"
   , dictLBracket: "{|"
   , dictRBracket: "|}"
   , dollar: "$"
   , dot: "."
   , ellipsis: ".."
   , else_: "else"
   , equals: "="
   , fun: "fun"
   , if_: "if"
   , import: "import"
   , in_: "in"
   , lArrow: "<-"
   , lBracket: "["
   , let_: "let"
   , lparenth: "("
   , match: "match"
   , rArrow: "->"
   , rBracket: "]"
   , rparenth: ")"
   , semiColon: ";"
   , then_: "then"
   , triplequote: "\"\"\""
   }
