module Temp.Parse where

import Control.Alt ((<|>))
import Parsing (Parser)
import Parsing.Language (emptyDef)
import Parsing.String (char)
import Parsing.String.Basic (oneOf)
import Parsing.Token (GenLanguageDef(..), LanguageDef, alphaNum, letter, unGenLanguageDef)
import Temp.Parse.Constants (keyword, ops, symbol)
import Util.Parse (SParser)

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef)
   { commentStart = "{-"
   , commentEnd = "-}"
   , commentLine = "--"
   , nestedComments = true
   , identStart = letter <|> char '_'
   , identLetter = alphaNum <|> oneOf [ '_', '\'' ]
   , opStart = opChar
   , opLetter = opChar
   , reservedOpNames = [ symbol.ellipsis ]
   , reservedNames = [ keyword.def ]
   , caseSensitive = true
   }

   where
   opChar :: Parser String Char
   opChar = oneOf ops
