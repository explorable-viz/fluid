module Parse where 

import Prelude hiding (between)
import Text.Parsing.Parser (ParserT)   
import Text.Parsing.Parser.Combinators (between)   
import Text.Parsing.Parser.String (string)   

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")
