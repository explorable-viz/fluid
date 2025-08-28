module Temp.Pretty.Helpers where

import Prelude

import Data.List (List(..), (:))
import Temp.Pretty.Constants (_lbracket, _rbracket, _lparen, _rparen, _quote)
import Temp.Pretty.Doc (Doc, text, (<++>))

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

parens :: Doc -> Doc
parens = enclose _lparen _rparen

brackets :: Doc -> Doc
brackets = enclose _lbracket _rbracket

quotes :: Doc -> Doc
quotes = enclose _quote _quote

string :: String -> Doc
string s = quotes (text s)

number :: forall a. Show a => a -> Doc
number s = text (show s)

todo :: String -> Doc
todo x = quotes (text ("TODO: " <> x))

vsep :: List Doc -> Doc
vsep Nil = mempty
vsep (d : Nil) = d
vsep (d : ds) = d <++> vsep ds
