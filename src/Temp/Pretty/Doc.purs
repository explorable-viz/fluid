module Temp.Pretty.Doc where

import Prelude

import Data.List (List(..), (:))
import Data.String as String
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
   | Record (List Doc)
   | Array (List Doc)

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
record = Record

array :: List Doc -> Doc
array = Array

-- Combinators
infixr 5 beside as <+>
infixr 5 above as <++>

beside :: Doc -> Doc -> Doc
beside a b = a <> text " " <> b

above :: Doc -> Doc -> Doc
above a b = a <> line <> b

replicate :: Int -> String -> String
replicate n s
   | n <= 0 = ""
   | otherwise = s <> replicate (n - 1) s

render :: Doc -> String
render = renderWithIndent 0

renderWithIndent :: Int -> Doc -> String
renderWithIndent n doc = case doc of
   Empty -> ""
   Line -> "\n" <> replicate (n * config.indentation) " "
   Text s -> s
   Indent d -> renderWithIndent (n + 1) d
   Concat d d' -> renderWithIndent n d <> renderWithIndent n d'
   d -> renderWithIndent n (simplify d)

simplify :: Doc -> Doc
simplify doc = case doc of
   Block d -> simpleBlock d
   Record fs -> simpleRecord fs
   Array fs -> simpleArray fs
   d -> d

simpleBlock :: Doc -> Doc
simpleBlock doc =
   if inline then text ": " <> simplify doc
   else text ":" <> indent (line <> simplify doc)
   where
   inline = case doc of
      Array _ -> true
      Record _ -> true
      _ -> inlinable doc && width doc < config.inlineBlockLimit

simpleRecord :: List Doc -> Doc
simpleRecord docs = case docs of
   Nil -> text "{}"
   ds ->
      if inline then
         text "{ " <> simples true ds <> text " }"
      else
         text "{" <> simples false ds <> line <> text "}"

      where
      inline = widthList ds < config.inlineRecordLimit

simpleArray :: List Doc -> Doc
simpleArray docs = case docs of
   Nil -> text "[]"
   ds ->
      if inline then
         text "[" <> simples true ds <> text "]"
      else
         text "[" <> simples false ds <> line <> text "]"

      where
      inline = widthList ds < config.inlineRecordLimit

simples :: Boolean -> List Doc -> Doc
simples inl fs = case fs of
   Nil -> mempty
   (x : Nil) ->
      if inl then
         simplify x
      else
         indent (line <> simplify x)
   (x : xs) ->
      if inl then
         (simplify x <> text ", ") <> simples inl xs
      else
         indent (line <> simplify x <> text ",") <> simples inl xs

inlinable :: Doc -> Boolean
inlinable doc = case doc of
   Line -> false
   Indent _ -> false
   Concat d1 d2 -> inlinable d1 && inlinable d2
   _ -> true

width :: Doc -> Int
width Empty = 0
width (Text s) = String.length s
width Line = 0 -- ???
width (Indent d) = width d
width (Concat d1 d2) = width d1 + width d2
width (Block d) = width d
width (Record ds) = widthList ds
width (Array ds) = widthList ds

widthList :: List Doc -> Int
widthList Nil = 0
widthList (x : Nil) = width x
widthList (x : xs) = width x + 2 + widthList xs
