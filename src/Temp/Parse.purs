module Temp.Parse (parsePy) where

import Prelude

import Control.Alt ((<|>))
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
import Parsing.Combinators (many, sepBy, sepBy1, try)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, withPos)
import Parsing.String (char, eof, string)
import SExpr (Clause(..), Expr(..), Pattern(..), VarDef(..))
import Temp.Parse.Parser (Parser, align, block, delim, floating, integer, lexeme, lines, parens, reserved, variable, whitespace)
import Temp.Util.UnsafeDebug (exitUnsafe, logErrorUnsafe)
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

expr :: Parser (Raw Expr)
expr = matchAs <|> ifElse <|> try funDef <|> valDef <|>  opTree
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
      opTree =  (buildExprParser opdefs simple)
         where
         simple :: Parser (Raw Expr)
         simple = try float <|> int <|> try appChain
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

program :: Parser (Raw Expr)
program = lines *> withPos expr <* whitespace <* eof

parsePy :: forall m. MonadError Error m => String -> m (Raw Expr)
parsePy input = liftEither $ lmap (evil input) $ runIndent $ runParserT input program

evil :: String -> ParseError -> Error
evil _ e@(ParseError msg (Position { line, column })) = do
   let e' = "Parse error at line " <> (show line) <> ", column " <> (show column) <> ":\n" <> msg
   let _ = logErrorUnsafe e'
   let _ = exitUnsafe unit
   error (show e)
