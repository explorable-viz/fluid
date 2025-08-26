module Temp.Parse (parsePy) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT)
import Data.Bifunctor (lmap)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.Traversable (foldl)
import Doc (DocOpt(..))
import Effect.Exception (Error, error)
import Lattice (Raw)
import Parsing (ParseError(..), Position(..), runParserT)
import Parsing.Combinators (sepBy, sepBy1)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, withPos)
import Parsing.String (char, eof, string)
import SExpr (Clause(..), Expr(..), Pattern(..))
import Temp.Parse.Parser (Parser, align, block, delim, floating, integer, lexeme, lines, parens, reserved, unreserved, whitespace)
import Temp.Util.UnsafeDebug (exitUnsafe, logErrorUnsafe)
import Util (nonEmpty, (×))

variable :: Parser (Raw Expr)
variable = unreserved <#> Var

int :: Parser (Raw Expr)
int = integer <#> Int unit None

float :: Parser (Raw Expr)
float = floating <#> Float unit None

pvar :: Parser Pattern
pvar = unreserved <#> PVar

binaryOp :: String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
binaryOp op = do
   op' <- lexeme $ string op
   pure $ \e e' -> BinaryApp e op' e'

backtickOp :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
backtickOp = do
   x <- delim '(' *> unreserved <* delim ')'
   pure (\e e' -> BinaryApp e x e')

opdefs :: Array (Array (Operator (StateT Position Identity) String (Raw Expr)))
opdefs =
   [ [ Infix backtickOp AssocLeft ]
   , [ Infix (binaryOp ".") AssocLeft
     , Infix (binaryOp "!") AssocLeft
     , Infix (binaryOp "**") AssocRight
     ]
   , [ Infix (binaryOp "*") AssocLeft
     , Infix (binaryOp "/") AssocLeft
     ]
   , [ Infix (binaryOp "+") AssocLeft
     , Infix (binaryOp "-") AssocLeft
     ]
   , [ Infix (binaryOp "++") AssocRight ]
   , [ Infix (binaryOp "==") AssocNone
     , Infix (binaryOp "/=") AssocNone
     , Infix (binaryOp "<") AssocLeft
     , Infix (binaryOp ">") AssocLeft
     , Infix (binaryOp "<=") AssocLeft
     , Infix (binaryOp ">=") AssocLeft
     ]
   ]

expression :: Parser (Raw Expr)
expression =
   fix (opTreeLeaf >>> buildExprParser opdefs)

   where

   opTreeLeaf :: Parser (Raw Expr) -> Parser (Raw Expr)
   opTreeLeaf expr' = conditional <|> def <|> appChain
      where

      conditional :: Parser (Raw Expr)
      conditional = do
         reserved "if"
         c <- expr'
         t <- block expr'
         align (reserved "else")
         e <- block expr'
         pure $ IfElse c t e

      def :: Parser (Raw Expr)
      def = do
         reserved "def"
         name <- unreserved
         ps <- params
         e <- block expr'
         e' <- align expr'
         pure $ LetRec (nonEmpty ((name × Clause (ps × e)) : Nil)) e'

         where

         params :: Parser (NonEmptyList Pattern)
         params = parens $ sepBy1 pvar (lexeme $ char ',')

      appChain :: Parser (Raw Expr)
      appChain = simple >>= \e -> app e

         where

         app :: Raw Expr -> Parser (Raw Expr)
         app e = args e <|> pure e

         args :: Raw Expr -> Parser (Raw Expr)
         args e = do
            ps <- parens $ sepBy expr' (lexeme $ char ',')
            app (foldl (App None) e ps)

         simple :: Parser (Raw Expr)
         simple = variable <|> float <|> int

program :: Parser (Raw Expr)
program = lines *> withPos expression <* whitespace <* eof

parsePy :: forall m. MonadError Error m => String -> m (Raw Expr)
parsePy input = liftEither $ lmap (evil input) $ runIndent $ runParserT input program

evil :: String -> ParseError -> Error
evil _ e@(ParseError msg (Position { line, column })) = do
   let e' = "Parse error at line " <> (show line) <> ", column " <> (show column) <> ":\n" <> msg
   let _ = logErrorUnsafe e'
   let _ = exitUnsafe unit
   error (show e)
