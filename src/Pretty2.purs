module Pretty2 where

import Prelude
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..),Pattern(..), Clauses(..), Clause(..), RecDefs, Branch)
import Data.List (List(..))
import Bindings (Bind, key, val)
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Foldable(foldl)
import Util ((×), error, type(×))

infixl 5 beside as :<>:
infixl 5 atop as .-.

emptyDoc :: Doc
emptyDoc = empty 0 0

data InFront a =  Prefix (Expr a) | Unit

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n)
pretty (Var x) = text x
pretty (App s s') = pretty s :<>: pretty s'
pretty (BinaryApp s x s') = pretty s :<>: text x :<>: pretty s'
pretty (IfElse s s_1 s_2) = text "if" :<>: pretty s :<>: text "then" :<>: pretty s_1 :<>: text "else" :<>: pretty s_2
pretty (Project s x) = pretty s :<>: text "." :<>: text x
pretty (Record _ x) = text "{" :<>: prettyAuxillaryFunc x :<>: text "}" -- formatting needs fixing 
pretty (Lambda (Clauses cs)) = text "fun" :<>: text "{" :<>: prettyAuxillaryFuncClauses Unit (Clauses cs) :<>: text "}"  
-- pretty (MatchAs s x) = text "match" :<>: pretty s :<>: text "as" :<>: prettyAuxillaryFuncClauses Unit (Clauses (Clause x))
pretty _ = emptyDoc

-- subtle error in code needs fixing
prettyAuxillaryFunc :: forall a. List (Bind (Expr a)) -> Doc
prettyAuxillaryFunc (Cons x xs) = text (key x) :<>: text ":" :<>: pretty (val x) .-. prettyAuxillaryFunc xs
prettyAuxillaryFunc Nil = emptyDoc

prettyAuxillaryFuncPattern :: Pattern -> Doc 
prettyAuxillaryFuncPattern (PVar x) = text x 
prettyAuxillaryFuncPattern (PRecord x) = text "{" :<>: prettyAuxillaryFuncVarPatt x :<>: text "}"
prettyAuxillaryFuncPattern (PConstr c x) = text c :<>: text "(" :<>: prettyAuxillaryFuncPatterns x :<>: text ")"
prettyAuxillaryFuncPattern _ = emptyDoc

prettyAuxillaryFuncVarPatt :: List (Bind (Pattern)) -> Doc
prettyAuxillaryFuncVarPatt (Cons x xs) = text (key x) :<>: text ":" :<>: prettyAuxillaryFuncPattern (val x) .-. prettyAuxillaryFuncVarPatt xs
prettyAuxillaryFuncVarPatt Nil = emptyDoc

-- in Tex file Patt is more than one pattern but here it can be non-empty (might need to fix this)
prettyAuxillaryFuncPatterns :: List Pattern -> Doc
prettyAuxillaryFuncPatterns (Cons x xs) = prettyAuxillaryFuncPattern x .-. prettyAuxillaryFuncPatterns xs 
prettyAuxillaryFuncPatterns Nil = emptyDoc 

unitClauses :: forall a. Clause a -> Doc 
unitClauses (Clause (ps × e)) = prettyAuxillaryFuncPatterns (toList ps):<>: text "=" :<>: pretty e

varClauses :: forall a. Expr a -> Clause a -> Doc 
varClauses (Var x) (Clause (ps × e)) = text x :<>: prettyAuxillaryFuncPatterns (toList ps):<>: text "=" :<>: pretty e
varClauses _ _ = emptyDoc

prettyAuxillaryFuncClauses :: forall a. InFront a -> Clauses a -> Doc 
prettyAuxillaryFuncClauses Unit (Clauses cs) = let docs = map unitClauses cs in foldl (.-.) emptyDoc docs 
prettyAuxillaryFuncClauses (Prefix p@(Var _x)) (Clauses cs) = let docs = map (varClauses p) cs in foldl (.-.) emptyDoc docs 
prettyAuxillaryFuncClauses _ _ = emptyDoc

--helperFunctionTemp :: forall a. List (Branch a) -> Doc 
--helperFunctionTemp (Cons x xs) = (varClauses (key x) (val x)) .-. (helperFunctionTemp xs)
--helperFunctionTemp Nil = emptyDoc
--helperFunctionTemp

--prettyRecursiveFunc :: forall a. RecDefs a -> Doc 
--prettyRecursiveFunc x = helperFunctionTemp (toList x)
--prettyRecursiveFunc _ = emptyDoc

-- prettyRecursiveFunc (RecDefs x) = let docs = map varClauses2 x in foldl (.-.) emptyDoc docs 

auxillaryFunction1 :: forall a. RecDefs a -> NonEmptyList (NonEmptyList (Branch a))
auxillaryFunction1 _ = error "to do"

auxillaryFunction2 :: forall a. NonEmptyList (NonEmptyList (Branch a)) -> NonEmptyList (NonEmptyList (String × Clauses a))
auxillaryFunction2 _ = error "to do"
