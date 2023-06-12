module Pretty2 where

import Prelude
import Text.Pretty (Doc, empty, text, beside, atop)
import SExpr (Expr(..), Pattern(..), Clauses(..), Clause(..), RecDefs, Branch, ListRest(..))
import Data.List (List(..))
import Bindings (Bind, key, val)
import Data.List.NonEmpty (NonEmptyList, toList, groupBy, head, singleton)
import Data.Foldable (foldl)
import Util ((×), type (×))

infixl 5 beside as :<>:
infixl 5 atop as .-.
data InFront = Prefix (String) | Unit

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n)
pretty (Var x) = text x
pretty (App s s') = pretty s :<>: pretty s'
pretty (BinaryApp s x s') = pretty s :<>: text x :<>: pretty s'
pretty (IfElse s s_1 s_2) = text "if" :<>: pretty s :<>: text "then" :<>: pretty s_1 :<>: text "else" :<>: pretty s_2
pretty (Project s x) = pretty s :<>: text "." :<>: text x
pretty (Record _ x) = text "{" :<>: prettyAuxillaryFuncVarExpr x :<>: text "}" -- formatting needs fixing 
pretty (Lambda (Clauses cs)) = text "fun" :<>: text "{" :<>: prettyAuxillaryFuncClauses Unit (Clauses cs) :<>: text "}"
pretty (LetRec g s) = text "let" :<>: combiningAuxillaryFunctionsRec g :<>: text "in" :<>: pretty s
pretty (MatchAs s x) = text "match" :<>: pretty s :<>: text "as" :<>: combiningMatch x
pretty (ListEmpty _) = text "[]"
pretty (ListNonEmpty _ s x) = text "[" :<>: pretty s :<>: listAuxillaryFunc x :<>: text "]"
pretty _ = emptyDoc

listAuxillaryFunc :: forall a. ListRest a -> Doc
listAuxillaryFunc (Next _ s x) = text "," :<>: pretty s :<>: listAuxillaryFunc x
listAuxillaryFunc (End _) = emptyDoc

matchAuxillaryFunc1 :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
matchAuxillaryFunc1 x = map matchAuxillaryFunc11 x

matchAuxillaryFunc11 :: forall a. Pattern × Expr a -> NonEmptyList Pattern × Expr a
matchAuxillaryFunc11 (a × b) = singleton a × b

matchAuxillaryFunc2 :: forall a. NonEmptyList (NonEmptyList Pattern × Expr a) -> NonEmptyList (Clause a)
matchAuxillaryFunc2 x = map (\y -> Clause y) x

matchAuxillaryFunc3 :: forall a. NonEmptyList (Clause a) -> Clauses a
matchAuxillaryFunc3 x = Clauses x

combiningMatch :: forall a. NonEmptyList (Pattern × Expr a) -> Doc
combiningMatch x = prettyAuxillaryFuncClauses Unit (matchAuxillaryFunc3 (matchAuxillaryFunc2 (matchAuxillaryFunc1 x)))

-- subtle error in code needs fixing
prettyAuxillaryFuncVarExpr :: forall a. List (Bind (Expr a)) -> Doc
prettyAuxillaryFuncVarExpr (Cons x xs) = text (key x) :<>: text ":" :<>: pretty (val x) .-. prettyAuxillaryFuncVarExpr xs
prettyAuxillaryFuncVarExpr Nil = emptyDoc

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
prettyAuxillaryFuncPatterns (Cons x xs) = prettyAuxillaryFuncPattern x :<>: prettyAuxillaryFuncPatterns xs -- beside may need to change 
prettyAuxillaryFuncPatterns Nil = emptyDoc

unitClauses :: forall a. Clause a -> Doc
unitClauses (Clause (ps × e)) = prettyAuxillaryFuncPatterns (toList ps) :<>: text "=" :<>: pretty e

varClauses :: forall a. String -> Clause a -> Doc
varClauses x (Clause (ps × e)) = text x :<>: prettyAuxillaryFuncPatterns (toList ps) :<>: text "=" :<>: pretty e

prettyAuxillaryFuncClauses :: forall a. InFront -> Clauses a -> Doc
prettyAuxillaryFuncClauses Unit (Clauses cs) = let docs = map unitClauses cs in foldl (:<>:) emptyDoc docs -- beside may need to change
prettyAuxillaryFuncClauses (Prefix x) (Clauses cs) = let docs = map (varClauses x) cs in foldl (:<>:) emptyDoc docs

auxillaryFunction1Rec :: forall a. RecDefs a -> NonEmptyList (NonEmptyList (Branch a))
auxillaryFunction1Rec x = groupBy (\p q -> key p == key q) x

-- have not wrapped around Clauses yet 
auxillaryFunction21Rec :: forall a. NonEmptyList (Branch a) -> String × NonEmptyList (Clause a)
auxillaryFunction21Rec x = key (head x) × map (\y -> val y) x

auxillaryFunction2Rec :: forall a. NonEmptyList (NonEmptyList (Branch a)) -> NonEmptyList (String × NonEmptyList (Clause a))
auxillaryFunction2Rec x = map auxillaryFunction21Rec x

auxillaryFunction31Rec :: forall a. String × NonEmptyList (Clause a) -> Doc
auxillaryFunction31Rec (a × b) = prettyAuxillaryFuncClauses (Prefix a) (Clauses b)

auxillaryFunction3Rec :: forall a. NonEmptyList (String × NonEmptyList (Clause a)) -> Doc
auxillaryFunction3Rec x = let docs = map auxillaryFunction31Rec x in foldl (.-.) emptyDoc docs

combiningAuxillaryFunctionsRec :: forall a. RecDefs a -> Doc
combiningAuxillaryFunctionsRec x = auxillaryFunction3Rec (auxillaryFunction2Rec (auxillaryFunction1Rec x))