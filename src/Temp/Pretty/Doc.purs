module Temp.Pretty.Doc where

import Prelude

import Data.List (List(..), (:))
import Data.String as String
import Data.Traversable (intercalate)
import Temp.Pretty.Config (config)

data Doc
   -- base doc
   = Empty
   | Line
   | Text String
   | Indent Doc
   | Concat Doc Doc
   -- fancy doc
   | Block Doc
   | Collection Collection (List Doc)

data Collection = Record | Array

data Format = Inline | Multiline

instance Semigroup Doc where
   append = Concat

instance Monoid Doc where
   mempty = Empty

text :: String -> Doc
text = Text

line :: Doc
line = Line

indent :: Doc -> Doc
indent = Indent

block :: Doc -> Doc
block = Block

record :: List Doc -> Doc
record = Collection Record

array :: List Doc -> Doc
array = Collection Array

-- Combinators
infixr 5 beside as <+>
infixr 5 above as <++>
infixr 5 above2 as <+++>

beside :: Doc -> Doc -> Doc
beside a b = a <> text " " <> b

above :: Doc -> Doc -> Doc
above a b = a <> line <> b

above2 :: Doc -> Doc -> Doc
above2 a b = a <> (line <> mempty) <> line <> b

enclosed :: Doc -> Doc -> Doc -> Doc
enclosed l r d = l <> d <> r

between :: Doc -> Doc -> Doc -> Doc
between l r d = l <+> d <+> r

spaces :: Int -> String
spaces n
   | n <= 0 = ""
   | otherwise = " " <> spaces (n - 1)

render :: Doc -> String
render = renderWithIndent 0

renderWithIndent :: Int -> Doc -> String
renderWithIndent n doc = case doc of
   Empty -> ""
   Line -> "\n" <> spaces (n * config.indentation)
   Text s -> s
   Indent d -> renderWithIndent (n + 1) d
   Concat Line Empty -> "\n"
   Concat d d' -> renderWithIndent n d <> renderWithIndent n d'
   d -> renderWithIndent n (simplify d)

simplify :: Doc -> Doc
simplify doc = case doc of
   Block d -> case fmt of
      Inline -> text ": " <> simplify d
      Multiline -> text ":" <> indent (line <> simplify d)
   Collection c ds -> delimit fmt c $ simplifyList ds fmt
   d -> d

   where
   fmt = format doc

   delimit Inline Record = inside "{ " " }"
   delimit Multiline Record = inside "{" "}"
   delimit _ Array = inside "[" "]"

   inside l r d = text l <> d <> text r

simplifyList :: List Doc -> Format -> Doc
simplifyList ds fmt = case fmt of
   Inline -> intercalate (text ", ") ((\d -> simplify d) <$> ds)
   Multiline -> indent (intercalate (text ",") ((\d -> line <> simplify d) <$> ds)) <> line

format :: Doc -> Format
format doc = case doc of
   Block (Collection _ _) -> Inline
   Block d
      | inlinable d && width d < config.inlineBlockLimit -> Inline
      | otherwise -> Multiline
   Collection _ ds
      | widthList ds < config.inlineRecordLimit -> Inline
      | otherwise -> Multiline
   _ -> Inline

inlinable :: Doc -> Boolean
inlinable doc = case doc of
   Line -> false
   Indent _ -> false
   Concat d1 d2 -> inlinable d1 && inlinable d2
   _ -> true

width :: Doc -> Int
width Empty = 0
width Line = 0
width (Text s) = String.length s
width (Indent d) = width d
width (Concat d1 d2) = width d1 + width d2
width (Block d) = width d
width (Collection _ ds) = widthList ds

widthList :: List Doc -> Int
widthList Nil = 0
widthList (x : Nil) = width x
widthList (x : xs) = width x + 2 + widthList xs
