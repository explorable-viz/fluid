module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf, string)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Expr (Expr(..), Var)

type SParser = Parser String

-- constants
strIn = "in" :: String
strLet = "let" :: String

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = true
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf ['_', '\'']
                , opStart         = op'
                , opLetter        = op'
                , reservedOpNames = []
                , reservedNames   = []
                , caseSensitive   = true
                }
   where
      op' :: forall m . (Monad m) => ParserT String m Char
      op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

tokenParser :: TokenParser
tokenParser = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = tokenParser.reserved

variable :: SParser Expr
variable = ident >>= compose pure ExprVar

-- Need to resolve constructors vs. variables (https://github.com/explorable-viz/fluid/issues/49)
ident ∷ SParser Var
ident = tokenParser.identifier

-- JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
-- I'm assuming (but haven't checked that) DIGIT is equivalent to LanguageDef's "digit" parser.

{-
-- decimal-point = %x2E       ; .
const decimal_point = ch(".")
-- digit1-9 = %x31-39         ; 1-9
const digit1to9: Parser<string> = range("1", "9")
-- e = %x65 / %x45            ; e E
const e: Parser<string> = choice([ch("e"), ch("E")])
-- minus = %x2D               ; -
const minus: Parser<string> = ch("-")
-- plus = %x2B                ; +
const plus: Parser<string> = ch("+")
-- zero = %x30                ; 0
const zero: Parser<string> = ch("0")
-- exp = e [ minus / plus ] 1*DIGIT
const exp: Parser<string> = withJoin(sequence([e, optional(choice([minus, plus]), () => ""), withJoin(repeat1(DIGIT))]))
-- frac = decimal-point 1*DIGIT
const frac = withJoin(sequence([decimal_point, withJoin(repeat1(DIGIT))]))
-- int = zero / ( digit1-9 *DIGIT )
const int: Parser<string> = choice([zero, withJoin(sequence([digit1to9, withJoin(repeat(DIGIT))]))])
-- number = [ minus ] int [ frac ] [ exp ]
const numberʹ: Parser<string> =
   withJoin(sequence([optional(minus, () => ""), int, optional(frac, () => ""), optional(exp, () => "")]))
-}

int :: SParser Expr
int = tokenParser.integer >>= compose pure ExprInt

simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   let_ expr' <|>
   int
{- string {% id %} |
   parenthExpr {% id %} |
   pair {% id %} |
   list {% id %} |
   constr {% id %}
-}

let_ ∷ SParser Expr -> SParser Expr
let_ term' = do
   keyword strLet
   x ← ident
   e1 ← do
      tokenParser.reservedOp "="
      term'
   e2 ← do
      keyword strIn
      term'
   pure $ ExprLet x e1 e2

expr :: SParser Expr
expr = fix $ \p ->
   simpleExpr p
