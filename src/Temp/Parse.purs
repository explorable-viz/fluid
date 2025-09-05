module Temp.Parse (parsePy) where

import Prelude

import Bind (Var)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.State (StateT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Traversable (foldl)
import DataType (cPair)
import Doc (DocOpt(..))
import Lattice (Raw)
import Parsing (Position, runParserT)
import Parsing.Combinators (many, many1, sepBy, sepBy1, try, (<?>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, withPos)
import Parsing.String (char, eof, string)
import SExpr (Clause(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..))
import Temp.Parse.Parser (Parser, align, block, braces, brackets, constructor, delim, floating, integer, lexeme, lines, operator, parens, reserved, stringLiteral, variable, whitespace)
import Temp.Util.Error (prettyParseError)
import Util (type (×), nonEmpty, onlyIf, (×))

pattern :: Parser Pattern
pattern = defer $ \_ -> buildExprParser popdefs simplePattern

simplePattern :: Parser Pattern
simplePattern =
   try pListEmpty
      <|> pListNonEmpty
      <|> try pRecord
      <|> try pVar
      <|> try pAppChain
      <|> try parensPattern
      <|> pPair
   where

   pAppChain :: Parser Pattern
   pAppChain = pConstr >>= \e -> app e
      where
      app :: Pattern -> Parser Pattern
      app e = args e <|> pure e

      args :: Pattern -> Parser Pattern
      args (PConstr c ps) = do
         ps' <- parens $ sepBy simplePattern (lexeme $ char ',')
         app (PConstr c (ps <> ps'))
      args p = pure p

   pListEmpty :: Parser Pattern
   pListEmpty = brackets whitespace $> PListEmpty

   pListNonEmpty :: Parser Pattern
   pListNonEmpty = do
      delim '['
      head <- pattern
      rest <- pListRest
      pure $ PListNonEmpty head rest

      where
      pListRest :: Parser ListRestPattern
      pListRest = pListEnd <|> pListNext

         where
         pListEnd :: Parser ListRestPattern
         pListEnd = delim ']' $> PListEnd

         pListNext :: Parser ListRestPattern
         pListNext = do
            delim ','
            p <- pattern
            r <- pListRest
            pure $ PListNext p r

   pConstr :: Parser Pattern
   pConstr = PConstr <$> constructor <@> Nil

   pRecord :: Parser Pattern
   pRecord = do
      delim '{'
      fs <- sepBy pField (lexeme $ char ',')
      delim '}'
      pure $ PRecord fs

      where
      pField :: Parser (Var × Pattern)
      pField = do
         v <- variable
         delim ':'
         p <- pattern
         pure $ v × p

   pVar :: Parser Pattern
   pVar = PVar <$> variable

   parensPattern :: Parser Pattern
   parensPattern = do
      delim '('
      e <- pattern
      delim ')'
      pure $ e

   pPair :: Parser Pattern
   pPair = do
      delim '('
      p <- pattern
      delim ','
      p' <- pattern
      delim ')'
      pure $ PConstr cPair (p : p' : Nil)

binaryOp :: String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
binaryOp op = try do
   op' <- lexeme $ operator
   onlyIf (op == op')
      $ \e e' -> BinaryApp e op' e'

pBinaryOp :: String -> Parser (Pattern -> Pattern -> Pattern)
pBinaryOp op = try do
   op' <- lexeme $ operator
   onlyIf (op == op')
      $ \e e' -> PConstr op' (e : e' : Nil)

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
   , [ Infix (binaryOp ":|") AssocRight ]
   , [ Infix (binaryOp "++") AssocRight ]
   , [ Infix (binaryOp "==") AssocNone
     , Infix (binaryOp "/=") AssocNone
     , Infix (binaryOp "<") AssocLeft
     , Infix (binaryOp ">") AssocLeft
     , Infix (binaryOp "<=") AssocLeft
     , Infix (binaryOp ">=") AssocLeft
     ]
   ]

popdefs :: Array (Array (Operator (StateT Position Identity) String Pattern))
popdefs = [ [ Infix (pBinaryOp ":|") AssocRight ] ]

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
      params = parens $ sepBy1 pattern (lexeme $ char ',')

   valDef :: Parser (Raw Expr)
   valDef = do
      reserved "def"
      name <- pattern
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
            <|> try dict
            <|> try float
            <|> try int
            <|> try str
            <|> try projection
            <|> try appChain
            <|> try pair
            <|> try listComp
            <|> listEnum
            <|> try parensExpr
               <?> "expected simple"
         where

         projection :: Parser (Raw Expr)
         projection = try dprojection <|> rprojection
            where
            rprojection :: Parser (Raw Expr)
            rprojection = do
               e <- var
               delim '.'
               k <- variable
               pure $ Project None e k

            dprojection :: Parser (Raw Expr)
            dprojection = do
               e <- var
               delim '['
               k <- opTree
               delim ']'
               pure $ DProject None e k

         appChain :: Parser (Raw Expr)
         appChain = var <|> constr <|> parensOp >>= \e -> app e
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

            where
            kv :: Parser (Raw DictEntry × Raw Expr)
            kv = do
               k <- exprKey <|> varKey
               _ <- lexeme $ char ':'
               v <- opTree
               pure $ (k × v)

               where
               exprKey :: Parser (Raw DictEntry)
               exprKey = do
                  delim '['
                  e <- opTree
                  delim ']'
                  pure $ ExprKey e

               varKey :: Parser (Raw DictEntry)
               varKey = variable <#> VarKey unit

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

         listComp :: Parser (Raw Expr)
         listComp = do
            delim '['
            e <- opTree
            qs <- many1 qualifier
            delim ']'
            pure $ ListComp unit None e (toList qs)

            where
            qualifier :: Parser (Raw Qualifier)
            qualifier = listCompGen <|> listCompDecl <|> listCompGuard

               where
               listCompGen :: Parser (Raw Qualifier)
               listCompGen = do
                  reserved "for"
                  p <- pattern
                  reserved "in"
                  e <- opTree
                  pure $ ListCompGen None p e

               listCompDecl :: Parser (Raw Qualifier)
               listCompDecl = do
                  reserved "for"
                  p <- pattern
                  reserved "in"
                  e <- brackets $ opTree
                  pure $ ListCompDecl (VarDef p e)

               listCompGuard :: Parser (Raw Qualifier)
               listCompGuard = do
                  reserved "if"
                  e <- opTree
                  pure $ ListCompGuard e

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

         parensExpr :: Parser (Raw Expr)
         parensExpr = do
            delim '('
            e <- opTree
            delim ')'
            pure $ e

         parensOp :: Parser (Raw Expr)
         parensOp = parens operator <#> Op

program :: Parser (Raw Expr)
program = lines *> withPos expr <* whitespace <* eof

parsePy :: String -> Either String (Raw Expr)
parsePy input = lmap prettyParseError $ runIndent $ runParserT input program
