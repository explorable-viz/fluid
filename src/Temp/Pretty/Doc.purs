module Temp.Pretty.Doc where

import Prelude

import Data.String as String
import Data.Tuple (fst)
import Temp.Pretty.Config (config)
import Util (type (×), (×))

data Doc
   = Empty
   | Line
   | Text String
   | Indent Doc
   | Concat Doc Doc
   | Mode Mode Doc
   | StmtOrExpr Doc Doc
   | InlOrMul Doc Doc

data Mode = Stmt | Expr

data Format = Inline | Multiline

instance Semigroup Doc where
   append = Concat

instance Monoid Doc where
   mempty = Empty

empty :: Doc
empty = Empty

text :: String -> Doc
text = Text

line :: Doc
line = Line

indent :: Doc -> Doc
indent = Indent

stmt :: Doc -> Doc
stmt = Mode Stmt

expr :: Doc -> Doc
expr = Mode Expr

stmtOrExpr :: Doc -> Doc -> Doc
stmtOrExpr = StmtOrExpr

inlOrMul :: Doc -> Doc -> Doc
inlOrMul = InlOrMul

-- Combinators
infixr 5 beside as <+>
infixr 5 above as <++>
infixr 6 sep as </>

beside :: Doc -> Doc -> Doc
beside a b = a <> text " " <> b

above :: Doc -> Doc -> Doc
above a b = a <> line <> b

sep :: Doc -> Doc -> Doc
sep a b = inlOrMul (a <+> b) (a <++> b)

spaces :: Int -> String
spaces n
   | n <= 0 = ""
   | otherwise = " " <> spaces (n - 1)

render :: Doc -> String
render d = fst $ renderWithIndent Stmt 0 0 d

renderWithIndent :: Mode -> Int -> Int -> Doc -> (String × Int)
renderWithIndent m i w doc = case doc of
   Empty -> "" × w
   Line -> ("\n" <> spaces indentation) × indentation
   Concat Line Empty -> "\n" × 0
   Concat Line Line -> ("\n\n" <> spaces indentation) × indentation
   Text s -> s × (w + String.length s)
   Indent d -> renderWithIndent m (i + 1) w d
   Concat d1 d2 ->
      let
         (d1' × w') = renderWithIndent m i w d1
         (d2' × w'') = renderWithIndent m i w' d2
      in
         (d1' <> d2') × w''

   Mode m' d -> renderWithIndent m' i w d
   StmtOrExpr d1 d2 -> case m of
      Stmt -> renderWithIndent m i w d1
      Expr -> renderWithIndent m i w d2
   InlOrMul d1 d2 -> case fmt of
      Inline -> renderWithIndent m i w d1
      Multiline -> renderWithIndent m i w d2

   where
   fmt = format m w doc
   indentation = i * config.indentation

format :: Mode -> Int -> Doc -> Format
format m w doc
   | inlinable m doc && width m doc < (config.lineWidth - w) = Inline
   | otherwise = Multiline

inlinable :: Mode -> Doc -> Boolean
inlinable m doc = case doc of
   Empty -> true
   Line -> false
   Text _ -> true
   Indent _ -> false
   Concat d1 d2 -> inlinable m d1 && inlinable m d2
   Mode m' d -> inlinable m' d
   StmtOrExpr _ d -> case m of
      Stmt -> false
      Expr -> inlinable m d
   InlOrMul d _ -> inlinable m d

width :: Mode -> Doc -> Int
width m doc = case doc of
   Empty -> 0
   Line -> 0
   Text s -> String.length s
   Indent d -> width m d
   Concat d1 d2 -> width m d1 + width m d2
   Mode m' d -> width m' d
   StmtOrExpr d1 d2 -> case m of
      Stmt -> width m d1
      Expr -> width m d2
   InlOrMul d1 _ -> width m d1
