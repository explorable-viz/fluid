module Temp.Pretty.Doc (Doc(..), above, beside, block, record, array, line, render, text, (<++>), (<+>)) where

import Prelude

import Data.List (List(..), (:))
import Data.String as String

inlineRecordLimit :: Int
inlineRecordLimit = 50

-- todo tweak
inlineBlockLimit :: Int
inlineBlockLimit = 20

data Doc
   = Empty
   | Text String
   | Line
   | Concat Doc Doc
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

indentation :: String
indentation = "  "

replicate :: Int -> String -> String
replicate n s
   | n <= 0 = ""
   | otherwise = s <> replicate (n - 1) s

break :: Int -> String -> String
break n s = "\n" <> space n <> s

space :: Int -> String
space n = replicate n indentation

render :: Doc -> String
render doc = render' 0 doc

render' :: Int -> Doc -> String
render' _ Empty = ""
render' _ (Text s) = s
render' n Line = break n ""
render' n (Concat d1 d2) = render' n d1 <> render' n d2
render' n (Block d) = renderBlock n d
render' n (Record ds) = renderRecord n ds
render' n (Array ds) = renderArray n ds

renderArray :: Int -> List Doc -> String
renderArray n ds =
   if inline then
      "[" <> contents <> "]"
   else
      "[\n" <> contents <> break n "]"

   where
   inline :: Boolean
   inline = widthList ds < inlineRecordLimit

   contents :: String
   contents = renderList (n + 1) inline ds

renderRecord :: Int -> List Doc -> String
renderRecord _ Nil = "{}"
renderRecord n ds =
   if inline then
      "{ " <> contents <> " }"
   else
      "{\n" <> contents <> break n "}"
   where
   inline :: Boolean
   inline = widthList ds < inlineRecordLimit

   contents :: String
   contents = renderList (n + 1) inline ds

renderList :: Int -> Boolean -> List Doc -> String
renderList _ _ Nil = ""
renderList n inline (x : Nil) =
   if inline then
      render' n x
   else
      space n <> render' n x
renderList n inline (x : xs) =
   if inline then
      render' n x <> ", " <> renderList n inline xs
   else
      space n <> render' n x <> ",\n" <> renderList n inline xs

renderBlock :: Int -> Doc -> String
renderBlock n d =
   if inline then
      ": " <> render' n d
   else
      ":" <> break (n + 1) (render' (n + 1) d)
   where
   -- we should probably consider the current line width
   inline :: Boolean
   inline = case d of
      Array _ -> true
      Record _ -> true
      _ -> inlinable d && width d < inlineBlockLimit

inlinable :: Doc -> Boolean
inlinable Empty = true
inlinable (Text _) = true
inlinable Line = false
inlinable (Concat d1 d2) = inlinable d1 && inlinable d2
inlinable (Block _) = false
inlinable (Record _) = true
inlinable (Array _) = true

width :: Doc -> Int
width Empty = 0
width (Text s) = String.length s
width Line = 0 -- ???
width (Concat d1 d2) = width d1 + width d2
width (Block d) = width d
width (Record ds) = widthList ds
width (Array ds) = widthList ds

widthList :: List Doc -> Int
widthList Nil = 0
widthList (x : Nil) = width x
widthList (x : xs) = width x + 2 + widthList xs
