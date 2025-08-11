module Temp.Pretty.Helpers where

import Prelude
import Data.List (List(..), (:))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Temp.Pretty.Doc (Doc(..), text, indent, line, (<++>), (<+>))
import Temp.Pretty.Constants (_colon, _lbracket, _rbracket, _lparen, _rparen, _quote)

indentation :: String
indentation = "    "

num :: forall a. Show a => a -> Doc
num s = text (show s)

block :: Doc -> Doc
block d = _colon <> indent (line <> d)

var :: String -> Doc
var = text <<< replaceAll (Pattern "'") (Replacement "_")

op :: String -> Doc
op = text

constr :: String -> Doc
constr = text

replicate :: Int -> String -> String
replicate n s
   | n <= 0 = ""
   | otherwise = s <> replicate (n - 1) s

parens :: Doc -> Doc
parens d = _lparen <> d <> _rparen

brackets :: Doc -> Doc
brackets d = _lbracket <> d <> _rbracket

quotes :: Doc -> Doc
quotes d = _quote <> d <> _quote

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

render :: Doc -> String
render doc = renderWithIndent 0 doc
   where
   renderWithIndent :: Int -> Doc -> String
   renderWithIndent _ Empty = ""
   renderWithIndent _ (Text s) = s
   renderWithIndent n Line = "\n" <> replicate n indentation
   renderWithIndent n (Indent d) = renderWithIndent (n + 1) d
   renderWithIndent n (Concat d1 d2) =
      renderWithIndent n d1 <> renderWithIndent n d2
