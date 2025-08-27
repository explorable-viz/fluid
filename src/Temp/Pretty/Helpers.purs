module Temp.Pretty.Helpers where

import Prelude

import Data.List (List(..), (:))
import Temp.Pretty.Constants (_colon, _lbrace, _lbracket, _lparen, _quote, _rbrace, _rbracket, _rparen)
import Temp.Pretty.Doc (Doc(..), text, indent, line, (<++>), (<+>))

num :: forall a. Show a => a -> Doc
num s = text (show s)

block :: Doc -> Doc
block d = _colon <> indent (line <> d)

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

parens :: Doc -> Doc
parens = enclose _lparen _rparen

brackets :: Doc -> Doc
brackets = enclose _lbracket _rbracket

braces :: Doc -> Doc
braces = enclose _lbrace _rbrace

quotes :: Doc -> Doc
quotes = enclose _quote _quote

quotes' :: String -> Doc
quotes' s = quotes (text s)

todo :: String -> Doc
todo x = quotes (text ("TODO: " <> x))

vsep :: List Doc -> Doc
vsep Nil = mempty
vsep (d : Nil) = d
vsep (d : ds) = d <++> vsep ds

hsep :: List Doc -> Doc
hsep Nil = mempty
hsep (d : Nil) = d
hsep (d : ds) = d <+> hsep ds

hsepWith :: Doc -> List Doc -> Doc
hsepWith _ Nil = Empty
hsepWith _ (d : Nil) = d
hsepWith sep (d : ds) = d <> sep <> hsepWith sep ds
