module Pretty2 where

import Prelude
import Text.Pretty (Doc, empty, text)
import SExpr (Expr(..))


emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n) 
pretty (Var x) = text x
pretty (E.App e e') = beside (pretty e) (pretty e')
pretty _ = emptyDoc
