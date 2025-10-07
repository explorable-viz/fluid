module Temp.Parse (parsePy, parsePy', parsePyModule') where

import Prelude

import Bind (Var)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.State (StateT)
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either, choose)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.Traversable (foldl)
import DataType (cPair)
import Lattice (Raw)
import Parsing (Position, consume, runParserT)
import Parsing.Combinators (many, many1, optionMaybe, optional, sepBy, sepBy1, try, (<?>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Indent (runIndent, sameLine, withPos)
import Parsing.String (char, eof, satisfy, string)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Temp.Parse.Number (float, integer)
import Temp.Parse.Parser (Parser, align, block, brackets, constructor, context, delim, lexeme, lines, operator, reserved, stringLiteral, variable, whitespace)
import Temp.Util.Error (prettyParseError)
import Util (type (+), type (×), nonEmpty, onlyIf, (×))

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
         delim '('
         ps' <- sepBy simplePattern (lexeme $ char ',')
         delim ')'
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
   onlyIf (op == op') $
      -- else if ":|" op' then \e e' -> Constr unit op' (e : e' : empty)
      \e e' -> BinaryApp e op e'

pConsOp :: Parser (Pattern -> Pattern -> Pattern)
pConsOp = try do
   op <- lexeme $ operator
   onlyIf (op == ":|")
      $ \e e' -> PConstr ":" (e : e' : Nil)

infixFn :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
infixFn = try do
   x <- delim '|' *> variable <* delim '|'
   pure (\e e' -> BinaryApp e x e')

consOp :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
consOp = try do
   op <- lexeme $ operator
   onlyIf (op == ":|")
      $ \e e' -> Constr unit ":" (e : e' : Nil)

opdefs :: Array (Array (Operator (StateT Position Identity) String (Raw Expr)))
opdefs =
   [ [ Infix (binaryOp "!") AssocLeft
     , Infix (binaryOp "**") AssocRight
     ]
   , [ Infix (binaryOp "*") AssocLeft
     , Infix (binaryOp "/") AssocLeft
     ]
   , [ Infix (binaryOp "+") AssocLeft
     , Infix (binaryOp "-") AssocLeft
     ]
   , [ Infix consOp AssocRight ]
   , [ Infix (binaryOp "++") AssocRight ]
   , [ Infix (binaryOp "==") AssocNone
     , Infix (binaryOp "/=") AssocNone
     , Infix (binaryOp "<") AssocLeft
     , Infix (binaryOp ">") AssocLeft
     , Infix (binaryOp "<=") AssocLeft
     , Infix (binaryOp ">=") AssocLeft
     ]
   , [ Infix infixFn AssocLeft ]
   ]

popdefs :: Array (Array (Operator (StateT Position Identity) String Pattern))
popdefs = [ [ Infix pConsOp AssocRight ] ]

varDefs :: Parser (Raw VarDefs)
varDefs = do
   head <- varDef
   rest <- many varDef
   _ <- optional (delim ';')
   pure $ nonEmpty (head : rest)

   where
   varDef :: Parser (Raw VarDef)
   varDef = try do
      reserved "def"
      name <- pattern
      e <- block expr
      pure $ VarDef name e

recDefs :: Parser (Raw RecDefs)
recDefs = do
   head <- recDef
   rest <- many recDef
   _ <- optional (delim ';')
   pure $ nonEmpty (head : rest)

   where
   recDef :: Parser (Raw Branch)
   recDef = try do
      reserved "def"
      name <- variable
      delim '('
      ps <- sepBy1 pattern (lexeme $ char ',')
      delim ')'
      e <- block expr
      pure $ name × Clause (ps × e)

expr :: Parser (Raw Expr)
expr = context "expr" $ matchAs <|> ifElse <|> def <|> opTree <?> "expression"
   where
   matchAs :: Parser (Raw Expr)
   matchAs = do
      try $ reserved "match"
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
         try $ reserved "case"
         p <- pattern
         e <- block expr
         pure $ (p × e)

   def :: Parser (Raw Expr)
   def = context "def" do
      try funDef <|> valDef
      where
      funDef :: Parser (Raw Expr)
      funDef = context "funDef" $ withPos do
         defs' <- try recDefs
         e' <- align expr
         pure $ LetRec defs' e'

      valDef :: Parser (Raw Expr)
      valDef = context "valDef" $ withPos do
         defs' <- try varDefs
         e' <- align expr
         pure $ Let defs' e'

   ifElse :: Parser (Raw Expr)
   ifElse = do
      try $ reserved "if"
      c <- opTree
      t <- block expr
      align $ reserved "else"
      e <- block expr
      pure $ IfElse c t e

   opTree :: Parser (Raw Expr)
   opTree = context "opTree" (buildExprParser opdefs simpleOrProjection) <* consume -- this thing seems to break the `consume` state
      where

      simpleOrProjection :: Parser (Raw Expr)
      simpleOrProjection = simple >>= projection
         where
         projection :: Raw Expr -> Parser (Raw Expr)
         projection e = dprojection <|> rprojection <|> pure e
            where
            rprojection :: Parser (Raw Expr)
            rprojection = try do
               delim '.'
               k <- variable
               projection (Project e k)

            dprojection :: Parser (Raw Expr)
            dprojection = try do
               delim '['
               k <- opTree
               delim ']'
               projection (DProject e k)

      simple :: Parser (Raw Expr)
      simple = context "simple" $
         letExpr
            <|> letRecExpr
            <|> matrix
            <|> listExpr
            <|> lambda
            <|> dict
            <|> number
            <|> paragraph
            <|> str
            <|> pair
            <|> appChain
            <|> parensExpr
            <|> docExpr
               <?> "simple expression"
         where

         letExpr :: Parser (Raw Expr)
         letExpr = context "letExpr" do
            head <- varDef
            rest <- many varDef
            e' <- opTree
            pure $ Let (nonEmpty (head : rest)) e'
            where
            varDef :: Parser (Raw VarDef)
            varDef = try do
               reserved "def"
               name <- pattern
               delim ':'
               e <- opTree
               delim ';'
               pure $ VarDef name e

         letRecExpr :: Parser (Raw Expr)
         letRecExpr = context "letRecExpr" do
            head <- recDef
            rest <- many recDef
            e' <- opTree
            pure $ LetRec (nonEmpty (head : rest)) e'
            where
            recDef :: Parser (Raw Branch)
            recDef = try do
               reserved "def"
               name <- variable
               delim '('
               ps <- sepBy1 pattern (lexeme $ char ',')
               delim ')'
               delim ':'
               e <- opTree
               delim ';'
               pure $ name × Clause (ps × e)

         lambda :: Parser (Raw Expr)
         lambda = context "lambda" do
            try $ reserved "lambda"
            ps <- sepBy1 pattern (lexeme $ char ',')
            delim ':'
            e <- opTree
            pure $ Lambda (Clauses (nonEmpty (Clause (ps × e) : Nil)))

         appChain :: Parser (Raw Expr)
         appChain = context "app chain" $ withPos $
            var <|> constr <|> try parensExpr <|> try parensOp >>= \e -> app e
            where
            app :: Raw Expr -> Parser (Raw Expr)
            app e = sameLine *> args e <|> pure e

            args :: Raw Expr -> Parser (Raw Expr)
            args e = do
               try $ do
                  delim '('
                  sameLine
               ps <- sepBy opTree (lexeme $ char ',')
               delim ')'
               case e of
                  (Constr a c es) -> app (Constr a c (es <> ps <> Nil))
                  _ -> app (foldl App e ps)

         var :: Parser (Raw Expr)
         var = variable <#> Var

         constr :: Parser (Raw Expr)
         constr = constructor <#> Constr unit <*> pure Nil

         number :: Parser (Raw Expr)
         number = try (float <#> Float unit) <|> (integer <#> Int unit)

         str :: Parser (Raw Expr)
         str = stringLiteral <#> Str unit

         paragraph :: Parser (Raw Expr)
         paragraph = do
            _ <- lexeme $ string "f\"\"\""
            es <- many $ lexeme paragraphElem
            _ <- lexeme $ string "\"\"\""
            pure $ Paragraph es
            where
            paragraphElem :: Parser (Raw ParagraphElem)
            paragraphElem = token <|> unquote
               where
               token :: Parser (Raw ParagraphElem)
               token = do
                  cs <- some paragraphLetter
                  pure $ Token (SCU.fromCharArray cs)

                  where
                  -- TODO: allow escaped `"` and `{`
                  paragraphLetter :: Parser Char
                  paragraphLetter = satisfy $ \c -> (c /= '"' && c /= '{' && not (isSpace (codePointFromChar c)))

               unquote :: Parser (Raw ParagraphElem)
               unquote = do
                  delim '{'
                  e <- opTree
                  delim '}'
                  pure $ Unquote e

         dict :: Parser (Raw Expr)
         dict = context "dict" do
            delim '{'
            kvs <- sepBy kv (lexeme $ char ',')
            whitespace
            delim '}'
            pure $ Dictionary unit kvs

            where
            kv :: Parser (Raw DictEntry × Raw Expr)
            kv = do
               k <- exprKey <|> varKey
               _ <- lexeme $ char ':'
               v <- expr
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

         matrix :: Parser (Raw Expr)
         matrix = context "matrix" do
            _ <- try $ lexeme $ string "[|"
            e <- opTree
            reserved "for"
            delim '('
            x <- variable
            delim ','
            y <- variable
            delim ')'
            reserved "in"
            e' <- opTree
            _ <- lexeme $ string "|]"
            pure $ Matrix unit e (x × y) e'

         listExpr :: Parser (Raw Expr)
         listExpr = context "listExpr" do
            delim '['
            maybeExpr <- optionMaybe (try opTree)
            case maybeExpr of
               Nothing -> do
                  delim ']'
                  pure $ ListEmpty unit
               Just e -> listEnum e <|> listComp e <|> listNonEmpty e
            where

            listEnum :: Raw Expr -> Parser (Raw Expr)
            listEnum e = context "listEnum" do
               _ <- try $ lexeme $ string ".."
               e' <- opTree
               delim ']'
               pure $ ListEnum e e'

            listComp :: Raw Expr -> Parser (Raw Expr)
            listComp exp = context "listComp" do
               qs <- many1 (listCompGuard <|> listCompGenOrDecl)
               delim ']'
               pure $ ListComp unit exp (toList qs)

               where
               listCompGenOrDecl :: Parser (Raw Qualifier)
               listCompGenOrDecl = do
                  try $ reserved "for"
                  p <- pattern
                  reserved "in"
                  (listCompDecl' p <|> listCompGen' p)

                  where
                  listCompDecl' :: Pattern -> Parser (Raw Qualifier)
                  listCompDecl' p = do
                     e <- try $ brackets $ opTree
                     pure $ ListCompDecl (VarDef p e)

                  listCompGen' :: Pattern -> Parser (Raw Qualifier)
                  listCompGen' p = do
                     e <- opTree
                     pure $ ListCompGen p e

               listCompGuard :: Parser (Raw Qualifier)
               listCompGuard = do
                  try $ reserved "if"
                  e <- opTree
                  pure $ ListCompGuard e

            listNonEmpty :: Raw Expr -> Parser (Raw Expr)
            listNonEmpty head = do
               rest <- listRest
               pure $ ListNonEmpty unit head rest
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

         pair :: Parser (Raw Expr)
         pair = context "pair" $ do
            e <- try do
               delim '('
               e <- opTree
               delim ','
               pure e
            e' <- opTree
            delim ')'
            pure $ Constr unit cPair (e : e' : Nil)

         parensExpr :: Parser (Raw Expr)
         parensExpr = context "parens expr" do
            delim '('
            e <- opTree
            delim ')'
            pure $ e

         parensOp :: Parser (Raw Expr)
         parensOp = context "parens op" do
            delim '('
            op <- operator
            delim ')'
            pure $ Op op

         docExpr :: Parser (Raw Expr)
         docExpr = context "doc expr" do
            _ <- try $ lexeme $ string "@doc"
            delim '('
            e <- opTree
            delim ')'
            e' <- opTree
            pure $ DocExpr e e'

program :: Parser (Raw Expr)
program = lines *> withPos expr <* whitespace <* eof

defs :: Parser ((Raw VarDefs + Raw RecDefs))
defs = choose (try $ varDefs) (recDefs)

module_ :: Parser (Raw Module)
module_ = do
   defs' <- many (defs)
   pure $ Module defs'

imports_ :: Parser (List String)
imports_ = many (try $ reserved "import" *> modPath)
   where
   modPath :: Parser String
   modPath = joinWith "/" <<< fromFoldable <$> sepBy1 variable (delim '.')

topLevel :: forall a. Parser a -> Parser a
topLevel p = whitespace *> withPos p <* eof

withImports :: forall a. Parser a -> Parser (a × List String)
withImports p = topLevel do
   imports <- imports_
   a <- p
   pure $ a × imports

parsePy :: String -> Either String (Raw Expr)
parsePy input = lmap prettyParseError $ runIndent $ runParserT input program

parsePy' :: String -> Either String (Raw Expr × List String)
parsePy' input = lmap prettyParseError $ runIndent $ runParserT input (withImports expr)

parsePyModule' :: String -> Either String (Raw Module × List String)
parsePyModule' input = lmap prettyParseError $ runIndent $ runParserT input (withImports module_)
