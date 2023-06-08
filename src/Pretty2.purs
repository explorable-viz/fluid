module Pretty2 where

import Prelude 
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..))

infixl 5 beside as :<>:
infixl 5 atop as .-. 

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n) 
pretty (Var x) = text x
pretty (App s s') = pretty s :<>: pretty s'
pretty(BinaryApp s x s') = pretty s :<>: text x :<>: pretty s'

pretty _ = emptyDoc
