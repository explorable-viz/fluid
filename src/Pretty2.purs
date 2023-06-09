module Pretty2 where

import Prelude
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..),Pattern(..))
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
pretty (Record _ x) = text "{" :<>: prettyAuxillaryFunc x :<>: text "}" -- formatting needs fixing 
pretty _ = emptyDoc

data Tree a = Leaf | Node a (Tree a) (Tree a)


-- subtle error in code needs fixing
prettyAuxillaryFunc :: forall a. List (Bind (Expr a)) -> Doc
prettyAuxillaryFunc (Cons x xs) = text (key x) :<>: text ":" :<>: pretty (val x) .-. prettyAuxillaryFunc xs
prettyAuxillaryFunc Nil = emptyDoc

data InFront a = Expr a | Unit 

--prettyAuxillaryFuncClauses :: forall a. InFront a -> Clauses a -> Doc 
--prettyAuxillaryFuncClauses _ _ = emptyDoc
--prettyAuxillaryFuncClauses Unit (Cons x Nil) = 

prettyAuxillaryFuncPattern :: Pattern -> Doc 
prettyAuxillaryFuncPattern (PVar x) = text x 
prettyAuxillaryFuncPattern (PRecord x) = text "{" :<>: prettyAuxillaryFuncVarPatt x :<>: text "}"
prettyAuxillaryFuncPattern (PConstr c x) = text c :<>: text "(" :<>: prettyAuxillaryFuncPatterns x :<>: text ")"
prettyAuxillaryFuncPattern _ = emptyDoc

prettyAuxillaryFuncPatterns :: List Pattern -> Doc
prettyAuxillaryFuncPatterns (Cons x xs) = prettyAuxillaryFuncPattern x .-. prettyAuxillaryFuncPatterns xs 
prettyAuxillaryFuncPatterns Nil = emptyDoc 

prettyAuxillaryFuncVarPatt :: List (Bind (Pattern)) -> Doc
prettyAuxillaryFuncVarPatt (Cons x xs) = text (key x) :<>: text ":" :<>: prettyAuxillaryFuncPattern (val x) .-. prettyAuxillaryFuncVarPatt xs
prettyAuxillaryFuncVarPatt Nil = emptyDoc

