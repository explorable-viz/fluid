module Pretty2 where

import Prelude
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..),Pattern(..), Clauses(..), Clause(..), RecDefs, Branch)
import Data.List (List(..))
import Bindings (Bind, key, val)
import Data.List.NonEmpty (NonEmptyList, toList, groupBy, head, singleton)
import Data.Foldable(foldl)
import Util ((×), type(×))

infixl 5 beside as :<>:
infixl 5 atop as .-.

emptyDoc :: Doc
emptyDoc = empty 0 0

--data InFront a =  Prefix (Expr a) | Unit

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n)
pretty (Var x) = text x
pretty (App s s') = pretty s :<>: pretty s'
pretty (BinaryApp s x s') = pretty s :<>: text x :<>: pretty s'
pretty (IfElse s s_1 s_2) = text "if" :<>: pretty s :<>: text "then" :<>: pretty s_1 :<>: text "else" :<>: pretty s_2
pretty (Project s x) = pretty s :<>: text "." :<>: text x
pretty (Record _ x) = text "{" :<>: prettyAuxillaryFunc x :<>: text "}" -- formatting needs fixing 
pretty (Lambda (Clauses cs)) = text "fun" :<>: text "{" :<>: prettyAuxillaryFuncClauses Unit (Clauses cs) :<>: text "}" 
pretty (LetRec g s) = text "let" :<>: combiningAuxillaryFunctions g :<>: text "in" :<>: pretty s 
pretty (MatchAs s x) = text "match" :<>: pretty s :<>: text "as" :<>: combiningMatch x 
-- pretty (MatchAs s x) = text "match" :<>: pretty s :<>: text "as" :<>: prettyAuxillaryFuncClauses Unit (Clauses (Clause x))
pretty _ = emptyDoc

matchAuxillaryFunc11 :: forall a. Pattern × Expr a -> NonEmptyList Pattern × Expr a 
matchAuxillaryFunc11 (a × b) = singleton a × b 
-- Clauses = NonEmptyList (Clause) = NonEmptyList (NonEmptyList Pattern x Expr a)
matchAuxillaryFunc1 :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a) 
matchAuxillaryFunc1 x = map matchAuxillaryFunc11 x
-- matchAuxillaryFunc _ = error "to do"
matchAuxillaryFunc2 :: forall a. NonEmptyList (NonEmptyList Pattern × Expr a) -> NonEmptyList (Clause a)
matchAuxillaryFunc2 x = map (\y -> Clause y) x 

matchAuxillaryFunc3 :: forall a. NonEmptyList (Clause a) -> Clauses a 
matchAuxillaryFunc3 x = Clauses x 

combiningMatch :: forall a. NonEmptyList (Pattern × Expr a) -> Doc 
combiningMatch x = prettyAuxillaryFuncClauses Unit (matchAuxillaryFunc3 (matchAuxillaryFunc2 (matchAuxillaryFunc1 x))) 
-- in formula sheet says to use clauses function but unsure how to
-- since we have p1s1, p2s2 rather than p1p2p3... = s (definition of a clause)
-- matchAuxillaryFunc :: forall a. List (Pattern × Expr a) -> Doc
-- matchAuxillaryFunc (Cons x xs) = (prettyAuxillaryFuncPattern (key x) :<>: text "=" :<>: pretty (val x)) .-. matchAuxillaryFunc xs
-- matchAuxillaryFunc Nil = emptyDoc

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

--varClauses :: forall a. Expr a -> Clause a -> Doc 
--varClauses (Var x) (Clause (ps × e)) = text x :<>: prettyAuxillaryFuncPatterns (toList ps):<>: text "=" :<>: pretty e
--varClauses _ _ = emptyDoc

--prettyAuxillaryFuncClauses :: forall a. InFront a -> Clauses a -> Doc 
--prettyAuxillaryFuncClauses Unit (Clauses cs) = let docs = map unitClauses cs in foldl (.-.) emptyDoc docs 
--prettyAuxillaryFuncClauses (Prefix p@(Var _x)) (Clauses cs) = let docs = map (varClauses p) cs in foldl (.-.) emptyDoc docs 
--prettyAuxillaryFuncClauses _ _ = emptyDoc

data InFront =  Prefix (String) | Unit

varClauses :: forall a. String -> Clause a -> Doc 
varClauses x (Clause (ps × e)) = text x :<>: prettyAuxillaryFuncPatterns (toList ps):<>: text "=" :<>: pretty e
--varClauses _ _ = emptyDoc

prettyAuxillaryFuncClauses :: forall a. InFront -> Clauses a -> Doc 
prettyAuxillaryFuncClauses Unit (Clauses cs) = let docs = map unitClauses cs in foldl (.-.) emptyDoc docs 
prettyAuxillaryFuncClauses (Prefix x) (Clauses cs) = let docs = map (varClauses x) cs in foldl (.-.) emptyDoc docs 
--prettyAuxillaryFuncClauses _ _ = emptyDoc

--helperFunctionTemp :: forall a. List (Branch a) -> Doc 
--helperFunctionTemp (Cons x xs) = (varClauses (key x) (val x)) .-. (helperFunctionTemp xs)
--helperFunctionTemp Nil = emptyDoc
--helperFunctionTemp

--prettyRecursiveFunc :: forall a. RecDefs a -> Doc 
--prettyRecursiveFunc x = helperFunctionTemp (toList x)
--prettyRecursiveFunc _ = emptyDoc

-- prettyRecursiveFunc (RecDefs x) = let docs = map varClauses2 x in foldl (.-.) emptyDoc docs 

-- grouping by variable name 
-- RecDefs a  is just a Non Empty List of Var x Clause a
-- RecDefs = NonEmptyList (Branch a) 

auxillaryFunction1 :: forall a. RecDefs a -> NonEmptyList (NonEmptyList (Branch a))
auxillaryFunction1 x = groupBy (\p q -> key p == key q) x 

-- have not wrapped around Clauses yet 
auxillaryFunction21 :: forall a. NonEmptyList (Branch a) -> String × NonEmptyList (Clause a) 
auxillaryFunction21 x = key (head x) × map (\y -> val y) x
-- auxillaryFunction21 _ = error "to do"

auxillaryFunction2 :: forall a. NonEmptyList (NonEmptyList (Branch a)) -> NonEmptyList (String × NonEmptyList (Clause a))
auxillaryFunction2 x = map auxillaryFunction21 x

auxillaryFunction31 :: forall a. String × NonEmptyList (Clause a) -> Doc 
auxillaryFunction31 (a × b) = prettyAuxillaryFuncClauses (Prefix a) (Clauses b)

auxillaryFunction3 :: forall a. NonEmptyList (String × NonEmptyList (Clause a)) -> Doc
auxillaryFunction3 x = let docs = map auxillaryFunction31 x in foldl (.-.) emptyDoc docs 
-- auxillaryFunction3 _ = error "to do"

combiningAuxillaryFunctions :: forall a. RecDefs a -> Doc 
combiningAuxillaryFunctions x =  auxillaryFunction3 (auxillaryFunction2 (auxillaryFunction1 x)) 