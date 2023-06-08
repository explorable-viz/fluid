module Pretty2 where

import Text.Pretty (Doc, empty)
import SExpr (Expr)

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty _ = emptyDoc
