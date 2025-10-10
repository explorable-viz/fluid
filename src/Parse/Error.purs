module Parse.Error where

import Prelude

import Parsing (ParseError(..), Position(..))

prettyParseError :: ParseError -> String
prettyParseError (ParseError msg (Position { line, column })) = "ParseError on line " <> show line <> ", column " <> show column <> ":\n" <> msg
