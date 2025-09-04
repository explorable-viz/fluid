module Temp.Parse (parsePy) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (StateT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.Traversable (foldl)
import Doc (DocOpt(..))
import Lattice (Raw)
import Parsing (Position, runParserT)
import Parsing.Combinators (many, sepBy, sepBy1, try)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, withPos)
import Parsing.String (char, eof, string)
import SExpr (Clause(..), DictEntry(..), Expr(..), Pattern(..), VarDef(..))
import Temp.Parse.Parser (Parser, align, block, delim, floating, integer, lexeme, lines, parens, reserved, stringLiteral, variable, whitespace)
import Temp.Util.Error (prettyParseError)
import Util (type (×), nonEmpty, (×))

pvar :: Parser Pattern
pvar = variable <#> PVar

pattern :: Parser Pattern
pattern = simplePattern

simplePattern :: Parser Pattern
simplePattern = listEmpty <|> var
   where
   listEmpty :: Parser Pattern
   listEmpty = do
      _ <- lexeme $ string "[]"
      pure PListEmpty

   var :: Parser Pattern
   var = PVar <$> variable

binaryOp :: String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
binaryOp op = do
   op' <- lexeme $ string op
   pure $ \e e' -> BinaryApp e op' e'

backtickOp :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
backtickOp = do
   x <- delim '(' *> variable <* delim ')'
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

-- expr :=
--    | opTree
--    | Let expr expr
--    | LetRec expr expr
--    | MatchAs opTree [expr]
--    | IfElse opTree expr expr
--
-- opTree :=
--    | simple
--    | BinaryApp simple simple
--
-- simple :=
--    | (opTree)
--    | App opTree opTree
--    | ... everything else, all child expressions are opTree ...
--
-- identified requirements:
--
-- 1. different whitespace handling inside expr (with blocks) vs inside optree
--    (or maybe only inside lists/records/parens)
-- 2. simplify pattern such as in dict where we need to avoid left recursive functions
--    (maybe this is what `fix` is doing in original parser?)
-- 3. fix `++` operator not parsing correctly

expr :: Parser (Raw Expr)
expr = matchAs <|> ifElse <|> try funDef <|> valDef <|> opTree
   where
   matchAs :: Parser (Raw Expr)
   matchAs = do
      reserved "match"
      e <- opTree
      bs <- block branches
      pure $ MatchAs e bs
      where

      branches :: Parser (NonEmptyList (Pattern × Raw Expr))
      branches = do
         b <- branch
         bs <- many (try $ align branch)
         pure $ (nonEmpty (b : bs))

      branch :: Parser (Pattern × Raw Expr)
      branch = do
         reserved "case"
         p <- pattern
         e <- block expr
         pure $ (p × e)

   funDef :: Parser (Raw Expr)
   funDef = do
      reserved "def"
      name <- variable
      ps <- params
      e <- block expr
      e' <- align expr
      pure $ LetRec (nonEmpty ((name × Clause (ps × e)) : Nil)) e'
      where
      params :: Parser (NonEmptyList Pattern)
      params = parens $ sepBy1 pvar (lexeme $ char ',')

   valDef :: Parser (Raw Expr)
   valDef = do
      reserved "def"
      name <- pvar
      e <- block expr
      e' <- align expr
      pure $ Let (nonEmpty ((VarDef name e) : Nil)) e'

   ifElse :: Parser (Raw Expr)
   ifElse = do
      reserved "if"
      c <- opTree
      t <- block expr
      align (reserved "else")
      e <- block expr
      pure $ IfElse c t e

   opTree :: Parser (Raw Expr)
   opTree = (buildExprParser opdefs simple)
      where
      simple :: Parser (Raw Expr)
      simple = try float <|> try int <|> try string <|> try appChain <|> try dict
         where
         appChain :: Parser (Raw Expr)
         appChain = var >>= \e -> app e
            where
            app :: Raw Expr -> Parser (Raw Expr)
            app e = args e <|> pure e

            args :: Raw Expr -> Parser (Raw Expr)
            args e = do
               ps <- parens $ sepBy opTree (lexeme $ char ',')
               app (foldl (App None) e ps)

         var :: Parser (Raw Expr)
         var = variable <#> Var

         int :: Parser (Raw Expr)
         int = integer <#> Int unit None

         float :: Parser (Raw Expr)
         float = floating <#> Float unit None

         string :: Parser (Raw Expr)
         string = stringLiteral <#> Str unit None

         -- todo unuglify
         dict :: Parser (Raw Expr)
         dict = try empty <|> try nonEmpty
            where

            empty :: Parser (Raw Expr)
            empty = do
               _ <- lexeme $ char '{'
               _ <- lexeme $ char '}'
               pure $ Dictionary unit None Nil

            nonEmpty :: Parser (Raw Expr)
            nonEmpty = do
               kv <- first
               kvs <- many (try rest)
               whitespace
               _ <- lexeme $ char '}'
               pure $ Dictionary unit None ((kv : kvs))

               where
               first :: Parser ((Raw DictEntry) × (Raw Expr))
               first = do
                  _ <- lexeme $ char '{'
                  k <- opTree
                  _ <- lexeme $ char ':'
                  v <- opTree
                  pure $ (ExprKey k × v)

               rest :: Parser ((Raw DictEntry) × (Raw Expr))
               rest = do

                  _ <- lexeme $ char ','
                  k <- opTree
                  _ <- lexeme $ char ':'
                  v <- opTree
                  pure $ (ExprKey k × v)

program :: Parser (Raw Expr)
program = lines *> withPos expr <* whitespace <* eof

parsePy :: String -> Either String (Raw Expr)
parsePy input = lmap prettyParseError $ runIndent $ runParserT input program
