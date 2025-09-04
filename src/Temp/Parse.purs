module Temp.Parse (parsePy) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.State (StateT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.Traversable (foldl)
import DataType (cPair)
import Doc (DocOpt(..))
import Lattice (Raw)
import Parsing (Position, runParserT)
import Parsing.Combinators (many, sepBy, sepBy1, try, (<?>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, withPos)
import Parsing.String (char, eof, string)
import SExpr (Clause(..), DictEntry(..), Expr(..), ListRest(..), Pattern(..), VarDef(..))
import Temp.Parse.Parser (Parser, align, block, braces, brackets, constructor, delim, floating, integer, lexeme, lines, parens, reserved, stringLiteral, variable, whitespace)
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
-- 2. use fix or defer on left recursion
-- 3. fix `++` operator not parsing correctly

expr :: Parser (Raw Expr)
expr = matchAs <|> ifElse <|> try funDef <|> valDef <|> opTree <?> "expected expr"
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
      simple =
         try listEmpty
            <|> listNonEmpty
            <|> try constr
            <|> try dict
            <|> try float
            <|> try int
            <|> try str
            <|> try appChain
            <|> pair
            <|> listEnum
               <?> "expected simple"
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

         constr :: Parser (Raw Expr)
         constr = constructor <#> Constr unit None <*> pure Nil

         int :: Parser (Raw Expr)
         int = integer <#> Int unit None

         float :: Parser (Raw Expr)
         float = floating <#> Float unit None

         str :: Parser (Raw Expr)
         str = stringLiteral <#> Str unit None

         dict :: Parser (Raw Expr)
         dict = braces (sepBy kv (lexeme $ char ',')) <#> Dictionary unit None

         kv :: Parser (Raw DictEntry × Raw Expr)
         kv = defer \_ -> do
            k <- opTree
            _ <- lexeme $ char ':'
            v <- opTree
            whitespace
            pure $ (ExprKey k × v)

         listEmpty :: Parser (Raw Expr)
         listEmpty = brackets whitespace $> ListEmpty unit None

         listNonEmpty :: Parser (Raw Expr)
         listNonEmpty = do
            delim '['
            head <- opTree
            rest <- listRest
            pure $ ListNonEmpty unit None head rest

            where
            listRest :: Parser (Raw ListRest)
            listRest = listEnd <|> listNext

               where
               listEnd :: Parser (Raw ListRest)
               listEnd = delim ']' $> End unit

               listNext :: Parser (Raw ListRest)
               listNext = do
                  delim ','
                  e <- opTree
                  r <- listRest
                  pure $ Next unit e r

         listEnum :: Parser (Raw Expr)
         listEnum = do
            delim '['
            e <- opTree
            _ <- lexeme $ string ".."
            e' <- opTree
            delim ']'
            pure $ ListEnum e e'

         pair :: Parser (Raw Expr)
         pair = do
            delim '('
            e <- opTree
            delim ','
            e' <- opTree
            delim ')'
            pure $ Constr unit None cPair (e : e' : Nil)

program :: Parser (Raw Expr)
program = lines *> withPos expr <* whitespace <* eof

parsePy :: String -> Either String (Raw Expr)
parsePy input = lmap prettyParseError $ runIndent $ runParserT input program
