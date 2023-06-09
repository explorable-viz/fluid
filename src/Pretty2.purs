module Pretty2 where

import Prelude 
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..))
import Data.List (List(..))
import Bindings (Bind, key, val)


infixl 5 beside as :<>:
infixl 5 atop as .-. 

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n) 
pretty (Var x) = text x
pretty (App s s') = pretty s :<>: pretty s'
pretty (BinaryApp s x s') = pretty s :<>: text x :<>: pretty s'
pretty (IfElse s s_1 s_2) = text "if" :<>: pretty s :<>: text "then" :<>: pretty s_1 :<>: text "else" :<>: pretty s_2
pretty (Project s x) = pretty s :<>: text "." :<>: text x
pretty _ = emptyDoc

data Tree a = Leaf | Node a (Tree a) (Tree a) 

-- subtle error in code needs fixing
prettyAuxillaryFunc :: forall a. List (Bind (Expr a)) -> Doc
prettyAuxillaryFunc (Cons x xs) = text (key x) :<>: text ":" :<>: pretty (val x) .-. prettyAuxillaryFunc xs
prettyAuxillaryFunc Nil = emptyDoc

