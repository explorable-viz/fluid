module Temp.Pretty.Doc where

import Prelude

data Doc
   = Empty
   | Text String
   | Line
   | Indent Doc
   | Concat Doc Doc

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

-- Combinators
infixr 5 beside as <+>
infixr 5 above as <++>

beside :: Doc -> Doc -> Doc
beside a b = a <> text " " <> b

above :: Doc -> Doc -> Doc
above a b = a <> line <> b
