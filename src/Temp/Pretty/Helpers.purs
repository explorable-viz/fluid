module Temp.Pretty.Helpers where

import Prelude

import Data.List (List(..), (:))
import Temp.Pretty.Constants (_comma, _larray, _lbrace, _lbracket, _lparen, _quote, _rarray, _rbrace, _rbracket, _rparen)
import Temp.Pretty.Doc (Doc, indent, inlOrMul, line, stmtOrExpr, text, (<++>), (<+>), (</>))

block :: Doc -> Doc
block d = stmtOrExpr
   (text ":" <> inlOrMul (text " " <> d) (indent (line <> d)))
   (text ":" <> inlOrMul (text " " <> d) (indent (line <> d)))

record :: List Doc -> Doc
record ds = inlOrMul
   (text "{" <+> sep' (text ", ") ds <+> text "}")
   (text "{" <> indent (line <> sep' (text "," <> line) ds) <++> text "}")

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

parens :: Doc -> Doc
parens = enclose _lparen _rparen

braces :: Doc -> Doc
braces = enclose _lbrace _rbrace

brackets :: Doc -> Doc
brackets = enclose _lbracket _rbracket

matrix :: Doc -> Doc
matrix p = _larray </> p <> _rarray

pair :: forall a. (a -> Doc) -> a -> a -> Doc
pair f x y = parens (f x <> _comma <+> f y)

quotes :: Doc -> Doc
quotes = enclose _quote _quote

string :: String -> Doc
string s = quotes (text s)

number :: forall a. Show a => a -> Doc
number s = text (show s)

sep' :: Doc -> List Doc -> Doc
sep' _ Nil = mempty
sep' _ (d : Nil) = d
sep' s (d : ds) = d <> s <> sep' s ds

sep :: List Doc -> Doc
sep Nil = mempty
sep (d : Nil) = d
sep (d : ds) = d <> sep ds

vsep :: List Doc -> Doc
vsep Nil = mempty
vsep (d : Nil) = d
vsep (d : ds) = d <++> vsep ds

hsep :: List Doc -> Doc
hsep Nil = mempty
hsep (d : Nil) = d
hsep (d : ds) = d <+> hsep ds
