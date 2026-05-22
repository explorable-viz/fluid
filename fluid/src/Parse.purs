module Parse where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.State (StateT)
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either, choose)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (singleton, toList)
import Data.String (codePointFromChar)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.Traversable (foldl, foldr)
import DataType (cPair)
import Lattice (Raw)
import Parse.Number (float, integer)
import Parse.Parser (Parser, align, block, braces, brackets, close, commas, commas1, constructor, context, delim, fields, lexeme, operator, parens, reserved, reservedOperator, stringLiteral, trailingCommas, variable, whitespace)
import Parsing (ParseError(..), Position(..), consume, fail, runParserT)
import Parsing.Combinators (choice, many, many1, option, sepBy1, try, (<?>))
import Parsing.Expr (Assoc(..), Operator(..)) as P
import Parsing.Expr (Assoc(..), OperatorTable, buildExprParser)
import Parsing.Indent (runIndent, sameOrIndented, withPos)
import Parsing.String (eof, satisfy)
import Primitive.Parse (OpDef(..), OpType(..), Fixity(..), opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), RecDefs, Stmt(..), VarDef(..), VarDefs)
import Util (type (+), type (×), error, nonEmpty, (×))

pattern :: Parser Pattern
pattern = defer \_ -> buildExprParser [ [ P.Infix pConsOp P.AssocRight ] ] simplePattern

simplePattern :: Parser Pattern
simplePattern = pVar <|> pConstr <|> pRecord <|> pList <|> parensPattern
   where
   pVar :: Parser Pattern
   pVar = PVar <$> variable

   pConstr :: Parser Pattern
   pConstr = do
      c <- constructor
      ps <- option Nil (parens (commas simplePattern))
      pure $ PConstr c ps

   pRecord :: Parser Pattern
   pRecord = defer \_ -> braces (fields variable pattern) <#> PRecord

   pList :: Parser Pattern
   pList = defer \_ -> brackets (trailingCommas pattern) <#> case _ of
      Nil -> PListEmpty
      p : ps -> PListNonEmpty p (foldr PListNext PListEnd ps)

   parensPattern :: Parser Pattern
   parensPattern = do
      delim '('
      p <- pattern
      choice
         [ do
              delim ')'
              pure p
         , do
              delim ','
              p' <- pattern
              delim ')'
              pure $ PConstr cPair (p : p' : Nil)
         ]

pConsOp :: Parser (Pattern -> Pattern -> Pattern)
pConsOp = do
   reservedOperator ":|"
   pure \e e' -> PConstr ":" (e : e' : Nil)

varDefs :: Parser (Raw VarDefs)
varDefs = many1 varDef
   where
   varDef :: Parser (Raw VarDef)
   varDef = do
      p <- try (reserved "def" *> pattern <* reservedOperator "=")
      e <- sameOrIndented *> withPos expr
      pure $ VarDef p e

stmt :: Parser (Raw Stmt)
stmt = defer \_ -> ifStmt <|> matchStmt <|> defStmt <|> (reserved "return" *> expr <#> Return)

-- Top-level programs may omit 'return' on the trailing expression that gives
-- the program its value. Inside functions and other block bodies, 'return'
-- is required.
programStmt :: Parser (Raw Stmt)
programStmt = defer \_ -> ifStmt <|> matchStmt <|> programDefStmt <|> (reserved "return" *> expr <#> Return) <|> (Return <$> expr)

programDefStmt :: Parser (Raw Stmt)
programDefStmt = defer \_ -> defRecStmt <|> defValStmt
   where
   defRecStmt = defer \_ -> do
      ds <- recDefs
      body <- align programStmt
      pure $ DefRec ds body
   defValStmt = defer \_ -> do
      ds <- varDefs
      body <- align programStmt
      pure $ Def ds body

defStmt :: Parser (Raw Stmt)
defStmt = defer \_ -> defRecStmt <|> defValStmt
   where
   defRecStmt = defer \_ -> do
      ds <- recDefs
      body <- align stmt
      pure $ DefRec ds body
   defValStmt = defer \_ -> do
      ds <- varDefs
      body <- align stmt
      pure $ Def ds body

ifStmt :: Parser (Raw Stmt)
ifStmt = defer \_ -> do
   let
      ifClause = do
         c <- expr
         b <- blockBody
         pure (c × b)
   reserved "if"
   c <- ifClause
   cs <- many (align $ reserved "elif" *> ifClause)
   b <- align $ reserved "else" *> blockBody
   pure $ If (nonEmpty (c : cs)) b

matchStmt :: Parser (Raw Stmt)
matchStmt = defer \_ -> do
   let
      branch = do
         reserved "case"
         p <- pattern
         b <- blockBody
         pure (p × b)
   reserved "match"
   e <- expr
   bs <- block (many1 (align branch))
   pure $ Match e bs

blockBody :: Parser (Raw Stmt)
blockBody = defer \_ -> block stmt

recDefs :: Parser (Raw RecDefs)
recDefs = many1 recDef
   where
   recDef :: Parser (Raw Branch)
   recDef = do
      p <- try (reserved "def" *> variable <* delim '(')
      ps <- commas1 pattern
      delim ')'
      b <- blockBody
      pure $ p × Clause (ps × b)

expr :: Parser (Raw Expr)
expr = context "expr" $ ternary <?> "expression"
   where
   ternary :: Parser (Raw Expr)
   ternary = defer \_ -> do
      e1 <- opTree
      option e1 $ try do
         reserved "if"
         cond <- opTree
         reserved "else"
         e2 <- expr
         pure $ Ternary cond e1 e2

   opTree :: Parser (Raw Expr)
   opTree = context "opTree" (buildExprParser opTable simpleChain) <* consume -- otherwise always `consume: false`
      where

      opTable :: OperatorTable (StateT Position Identity) String (Raw Expr)
      opTable =
         opDefs # map (map toOperator)
         where
         toOperator :: OpDef -> P.Operator (StateT Position Identity) String (Raw Expr)
         toOperator (OpDef id fix opType) = case opType of
            Symbol -> op fix (reservedOperator id $> id)
            Ident -> op fix (reserved id $> id)
            CustomOp -> op (Infix AssocLeft) (try (delim '|' *> variable) <* delim '|')
            ConsOp -> P.Infix consOp AssocRight
            ProjectOp -> error "not implemented!"

         op :: Fixity -> Parser String -> P.Operator (StateT Position Identity) String (Raw Expr)
         op fix p = case fix of
            Infix assoc -> P.Infix (p <#> \id e e' -> BinaryApp e id e') assoc
            Prefix -> P.Prefix (p <#> \id e -> UnaryPrefixApp id e)
            Postfix -> error "not implemented!"

         consOp :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
         consOp = do
            reservedOperator ":|"
            pure \e e' -> Constr unit ":" (e : e' : Nil)

      simpleChain :: Parser (Raw Expr)
      simpleChain = withPos (simple >>= chain)
         where
         chain :: Raw Expr -> Parser (Raw Expr)
         chain e = sameOrIndented *> (project <|> dproject <|> app) <|> pure e
            where
            project :: Parser (Raw Expr)
            project = do
               -- try because '.' can be captured from '..'
               k <- try do
                  delim '.'
                  variable
               chain (Project e k)

            dproject :: Parser (Raw Expr)
            dproject = do
               delim '['
               k <- ternary
               close ']'
               chain (DProject e k)

            app :: Parser (Raw Expr)
            app = do
               delim '('
               ps <- commas ternary
               close ')'
               case e of
                  (Constr a c es) -> chain (Constr a c (es <> ps <> Nil))
                  _ -> chain (foldl App e ps)

      simple :: Parser (Raw Expr)
      simple = context "simple" $
         matrix
            <|> bracketsExpr
            <|> lambda
            <|> dict
            <|> paragraph
            <|> str
            <|> var
            <|> constr
            <|> parensExpr
            <|> docExpr
            <|> number
               <?> "simple expression"
         where

         lambda :: Parser (Raw Expr)
         lambda = context "lambda" do
            reserved "lambda"
            ps <- commas1 pattern
            delim ':'
            e <- ternary
            pure $ Lambda (Clauses (nonEmpty (Clause (ps × Return e) : Nil)))

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
            delim "f\"\"\""
            es <- many $ lexeme paragraphElem
            delim "\"\"\""
            pure $ Paragraph es
            where
            paragraphElem :: Parser (Raw ParagraphElem)
            paragraphElem = paragraphToken <|> unquote
               where
               paragraphToken :: Parser (Raw ParagraphElem)
               paragraphToken = do
                  cs <- some paragraphLetter
                  pure $ Token (SCU.fromCharArray cs)

                  where
                  -- TODO: allow escaped `"` and `{`
                  paragraphLetter :: Parser Char
                  paragraphLetter = satisfy $ \c -> (c /= '"' && c /= '{' && not (isSpace (codePointFromChar c)))

               unquote :: Parser (Raw ParagraphElem)
               unquote = defer $ \_ -> do
                  e <- braces (opTree)
                  pure $ Unquote e

         dict :: Parser (Raw Expr)
         dict = context "dict" do
            delim '{'
            kvs <- fields (exprKey <|> varKey) expr
            close '}'
            pure $ Dictionary unit kvs

            where
            exprKey :: Parser (Raw DictEntry)
            exprKey = defer \_ -> brackets ternary <#> ExprKey

            varKey :: Parser (Raw DictEntry)
            varKey = variable <#> VarKey unit

         matrix :: Parser (Raw Expr)
         matrix = context "matrix" do
            delim "[|"
            e <- ternary
            reserved "for"
            delim '('
            x <- variable
            delim ','
            y <- variable
            delim ')'
            reserved "in"
            e' <- ternary
            delim "|]"
            pure $ Matrix unit e (x × y) e'

         bracketsExpr :: Parser (Raw Expr)
         bracketsExpr = context "brackets" do
            delim '['
            choice
               [ do
                    close ']'
                    pure $ ListEmpty unit
               , do
                    e <- ternary
                    choice
                       [ context "listNonEmpty" do
                            delim ','
                            rest <- trailingCommas ternary
                            close ']'
                            pure $ ListNonEmpty unit e (foldr (Next unit) (End unit) rest)
                       , do
                            close ']'
                            pure $ ListNonEmpty unit e (End unit)
                       , context "listEnum" do
                            delim ".."
                            e' <- ternary
                            close ']'
                            pure $ ListEnum e e'

                       , context "listComp" do
                            qs <- many1 $ choice
                               [ context "listCompGuard" do
                                    reserved "if"
                                    e' <- opTree
                                    pure $ ListCompGuard e'
                               , context "listCompDecl" do
                                    reserved "def"
                                    p <- pattern
                                    delim ':'
                                    e' <- opTree
                                    pure $ ListCompDecl (VarDef p e')
                               , context "listCompGen" do
                                    reserved "for"
                                    p <- pattern
                                    reserved "in"
                                    e' <- opTree
                                    pure $ ListCompGen p e'
                               ]
                            close ']'
                            pure $ ListComp unit e (toList qs)
                       , fail "Expected `]"
                       ]
               , fail "Expected `]` or a list expression after `[`"
               ]

         parensExpr :: Parser (Raw Expr)
         parensExpr = context "parens" do
            delim '('
            choice
               [ do
                    op <- try (operator <* close ')')
                    pure $ Op op
               , do
                    e <- ternary
                    choice
                       [ do
                            close ')'
                            pure e
                       , do
                            delim ','
                            e' <- ternary
                            close ')'
                            pure $ Constr unit cPair (e : e' : Nil)
                       , fail "Expected `)` or `,` after `(expr`"
                       ]
               , fail "Expected `op` or `expr` after `(`"
               ]

         docExpr :: Parser (Raw Expr)
         docExpr = context "doc expr" do
            delim "@doc"
            e <- parens opTree
            e' <- opTree
            pure $ DocExpr e e'

defs :: Parser ((Raw VarDefs + Raw RecDefs))
defs = choose varDefs recDefs

module_ :: Parser (Raw Module)
module_ = do
   defs' <- many (defs)
   pure $ Module defs'

imports_ :: Parser (List String)
imports_ = many (reserved "import" *> modPath <* whitespace)
   where
   modPath :: Parser String
   modPath = joinWith "/" <<< fromFoldable <$> sepBy1 variable (delim '.')

topLevel :: forall a. Parser a -> Parser a
topLevel p = whitespace *> withPos p <* whitespace <* eof

withImports :: forall a. Parser a -> Parser (a × List String)
withImports p = topLevel do
   imports <- imports_
   a <- p
   pure $ a × imports

parse :: forall a. Parser a -> String -> Either String a
parse parser input =
   lmap printError $ runIndent $ runParserT input parser
   where
   printError :: ParseError -> String
   printError (ParseError msg (Position { line, column })) =
      "ParseError on line " <> show line <> ", column " <> show column <> ":\n" <> msg

parseProgram :: String -> Either String (Raw Stmt × List String)
parseProgram = parse (withImports programStmt)

parseModule :: String -> Either String (Raw Module × List String)
parseModule = parse (withImports module_)
