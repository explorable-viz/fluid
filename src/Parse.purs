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
import Data.List.NonEmpty (toList)
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
import Parsing.Expr (OperatorTable, buildExprParser)
import Parsing.Indent (runIndent, sameOrIndented, withPos)
import Parsing.String (eof, satisfy)
import Primitive.Parse (OpDef(..), OpParser(..), opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
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

infixSymbol :: String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
infixSymbol op = do
   reservedOperator op
   pure \e e' -> BinaryApp e op e'

infixIdent :: String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
infixIdent op = do
   reserved op
   pure \e e' -> BinaryApp e op e'

infixCustom :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
infixCustom = do
   fn <- try (delim '|' *> variable)
   delim '|'
   pure \e e' -> BinaryApp e fn e'

prefixIdent :: String -> Parser (Raw Expr -> Raw Expr)
prefixIdent op = do
   reserved op
   pure \e -> App (Var op) e

consOp :: Parser (Raw Expr -> Raw Expr -> Raw Expr)
consOp = do
   reservedOperator ":|"
   pure \e e' -> Constr unit ":" (e : e' : Nil)

opTable :: OperatorTable (StateT Position Identity) String (Raw Expr)
opTable =
   opDefs # map (map toOperator)
   where
   toOperator :: OpDef -> P.Operator (StateT Position Identity) String (Raw Expr)
   toOperator (Infix parser op assoc) = P.Infix (infixParser parser op) assoc
   toOperator (Prefix parser op) = P.Prefix (prefixParser parser op)
   toOperator (Postfix _ _) = error "not implemented!"

   infixParser :: OpParser -> String -> Parser (Raw Expr -> Raw Expr -> Raw Expr)
   infixParser Symbol op = infixSymbol op
   infixParser Ident op = infixIdent op
   infixParser ConsOp _ = consOp
   infixParser Custom _ = infixCustom

   prefixParser :: OpParser -> String -> Parser (Raw Expr -> Raw Expr)
   prefixParser Ident op = prefixIdent op
   prefixParser _ _ = error "not implemented!"

varDefs :: Parser (Raw VarDefs)
varDefs = many1 varDef
   where
   varDef :: Parser (Raw VarDef)
   varDef = do
      p <- try (reserved "def" *> pattern <* delim ':')
      e <- sameOrIndented *> withPos expr
      pure $ VarDef p e

recDefs :: Parser (Raw RecDefs)
recDefs = many1 recDef
   where
   recDef :: Parser (Raw Branch)
   recDef = do
      p <- try (reserved "def" *> variable <* delim '(')
      ps <- commas1 pattern
      delim ')'
      e <- block expr
      pure $ p × Clause (ps × e)

expr :: Parser (Raw Expr)
expr = context "expr" $ matchAs <|> ifElse <|> def <|> opTree <?> "expression"
   where
   matchAs :: Parser (Raw Expr)
   matchAs = do
      reserved "match"
      e <- opTree
      bs <- block (many1 (align branch))
      pure $ MatchAs e bs

      where
      branch :: Parser (Pattern × Raw Expr)
      branch = do
         reserved "case"
         p <- pattern
         e <- block expr
         pure (p × e)

   -- TODO: consider capturing 'def' parse for better error handling
   def :: Parser (Raw Expr)
   def = context "def" do
      funDef <|> valDef
      where
      funDef :: Parser (Raw Expr)
      funDef = context "funDef" $ withPos do
         ds <- recDefs
         e <- align expr
         pure $ LetRec ds e

      valDef :: Parser (Raw Expr)
      valDef = context "valDef" $ withPos do
         ds <- varDefs
         e <- align expr
         pure $ Let ds e

   ifElse :: Parser (Raw Expr)
   ifElse = do
      reserved "if"
      c <- clause
      cs <- many (align $ reserved "elif" *> clause)
      e <- align $ reserved "else" *> block expr
      pure $ IfElse (nonEmpty (c : cs)) e
      where
      clause = do
         c <- opTree
         e <- block expr
         pure (c × e)

   opTree :: Parser (Raw Expr)
   opTree = context "opTree" (buildExprParser opTable simpleChain) <* consume -- otherwise always `consume: false`
      where

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
               k <- opTree
               close ']'
               chain (DProject e k)

            app :: Parser (Raw Expr)
            app = do
               delim '('
               ps <- commas opTree
               close ')'
               case e of
                  (Constr a c es) -> chain (Constr a c (es <> ps <> Nil))
                  _ -> chain (foldl App e ps)

      simple :: Parser (Raw Expr)
      simple = context "simple" $
         letExpr
            <|> letRecExpr
            <|> matrix
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

         letExpr :: Parser (Raw Expr)
         letExpr = context "letExpr" do
            ds <- many1 varDef
            e <- opTree
            pure $ Let ds e
            where
            varDef :: Parser (Raw VarDef)
            varDef = do
               p <- try (reserved "def" *> pattern <* delim ':')
               e <- opTree
               delim ';'
               pure $ VarDef p e

         letRecExpr :: Parser (Raw Expr)
         letRecExpr = context "letRecExpr" do
            ds <- many1 recDef
            e <- opTree
            pure $ LetRec ds e
            where
            recDef :: Parser (Raw Branch)
            recDef = do
               p <- try (reserved "def" *> variable <* delim '(')
               ps <- commas1 pattern
               delim ')'
               delim ':'
               e <- opTree
               delim ';'
               pure $ p × Clause (ps × e)

         lambda :: Parser (Raw Expr)
         lambda = context "lambda" do
            reserved "lambda"
            ps <- commas1 pattern
            delim ':'
            e <- opTree
            pure $ Lambda (Clauses (nonEmpty (Clause (ps × e) : Nil)))

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
            exprKey = defer \_ -> brackets opTree <#> ExprKey

            varKey :: Parser (Raw DictEntry)
            varKey = variable <#> VarKey unit

         matrix :: Parser (Raw Expr)
         matrix = context "matrix" do
            delim "[|"
            e <- opTree
            reserved "for"
            delim '('
            x <- variable
            delim ','
            y <- variable
            delim ')'
            reserved "in"
            e' <- opTree
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
                    e <- opTree
                    choice
                       [ context "listNonEmpty" do
                            delim ','
                            rest <- trailingCommas opTree
                            close ']'
                            pure $ ListNonEmpty unit e (foldr (Next unit) (End unit) rest)
                       , do
                            close ']'
                            pure $ ListNonEmpty unit e (End unit)
                       , context "listEnum" do
                            delim ".."
                            e' <- opTree
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
                    e <- opTree
                    choice
                       [ do
                            close ')'
                            pure e
                       , do
                            delim ','
                            e' <- opTree
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

parseProgram :: String -> Either String (Raw Expr × List String)
parseProgram = parse (withImports expr)

parseModule :: String -> Either String (Raw Module × List String)
parseModule = parse (withImports module_)
