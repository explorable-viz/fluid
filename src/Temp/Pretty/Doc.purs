module Temp.Pretty.Doc where

import Prelude

import Data.List (List(..), (:))
import Data.String as String

inlineRecordLimit :: Int
inlineRecordLimit = 50

data Doc
   = Empty
   | Text String
   | Line
   | Indent Doc
   | Concat Doc Doc

   | Block Doc

   -- testing different handlings... probably revert
   | Record (List Doc)
   | Array (List Doc)
   | Params (List Doc)

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

params :: List Doc -> Doc
params = Params

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
render' n (Indent d) = render' (n + 1) d
render' n (Concat d1 d2) = render' n d1 <> render' n d2
render' n (Block d) = ":" <> break (n + 1) (render' (n + 1) d)
render' n (Array xs) = renderArray n xs
render' n (Record xs) = renderRecord n xs
render' n (Params xs) = renderParams n xs

between :: String -> String -> String -> String
between l r s = l <> s <> r

renderArray :: Int -> List Doc -> String
renderArray n elems = renderCollection "[" "]" n elems

renderRecord :: Int -> List Doc -> String
renderRecord n elems = renderCollection "{" "}" n elems

renderParams :: Int -> List Doc -> String
renderParams n elems = renderCollection "(" ")" n elems

renderCollection :: String -> String -> Int -> List Doc -> String
renderCollection l r n elems =
   if inline then
      l <> contents <> r
   else
      l <> "\n" <> contents <> break n r

   where
   inline :: Boolean
   inline = widthList elems < inlineRecordLimit

   contents :: String
   contents = renderList (n + 1) inline elems

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

width :: Doc -> Int
width Empty = 0
width (Text s) = String.length s
width Line = 0 -- ???
width (Indent _) = 0 -- ???
width (Concat d1 d2) = width d1 + width d2
width (Block d) = width d
width (Array xs) = widthList xs
width (Record xs) = widthList xs
width (Params xs) = widthList xs

widthList :: List Doc -> Int
widthList Nil = 0
widthList (x : Nil) = width x
widthList (x : xs) = width x + 2 + widthList xs
