module Pretty2 where

import Prelude

import Bindings (Bind, key, val)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList, toList, groupBy, head, singleton)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), RecDefs, VarDef(..), VarDefs, Qualifier(..))
import Util ((×), type (×))
import Util.Pair (Pair(..))
import Util.Pretty (Doc, atop, beside, beside3, empty, space, text)

infixl 5 atop as .-.
infixl 5 beside3 as .<>.
infixl 5 beside as :<>:
data InFront = Prefix (String) | Unit

infixl 5 space as :--:

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n) -- edited
-- pretty (App s s') = (pretty s .-. (emptyDoc :--: emptyDoc :--: emptyDoc :--: letArg s'))
pretty (Var x) = emptyDoc :--: text x :--: emptyDoc -- edited
pretty (Op x) = text "(" .<>. text x .<>. text ")"
pretty (App s s') = pretty s :--: pretty s'
pretty (BinaryApp s x s') = text "(" .<>. (pretty s :--: emptyDoc) .<>. (text x :--: emptyDoc) .<>. pretty s' .<>. text ")" -- edited
pretty (IfElse s s_1 s_2) = (emptyDoc :--: text "if" :--: emptyDoc) .<>. pretty s .<>. (emptyDoc :--: text "then" :--: emptyDoc) .<>. pretty s_1 .<>. (emptyDoc :--: text "else" :--: emptyDoc) .<>. pretty s_2
pretty (Project s x) = pretty s .<>. text "." .<>. text x
pretty (Record _ x) = text "{" .<>. prettyAuxillaryFuncVarExpr x .<>. text "}" -- formatting needs fixing 
pretty (Lambda (Clauses cs)) = text "(" .<>. (text "fun" :--: emptyDoc) .<>. prettyAuxillaryFuncClauses Unit (Clauses cs) .<>. text ")" :--: emptyDoc -- edited
pretty (LetRec g s) = text "let" :--: emptyDoc .<>. combiningAuxillaryFunctionsRec g .<>. (emptyDoc :--: text "in" :--: emptyDoc) .<>. pretty s
pretty (MatchAs s x) = text "match" .<>. pretty s .<>. text "as" .<>. combiningMatch x
pretty (ListEmpty _) = text "[]"
pretty (ListNonEmpty _ s x) = emptyDoc :--: text "[" .<>. pretty s .<>. listAuxillaryFunc x .<>. text "]" -- edited
pretty (ListEnum s s') = text "[" .<>. pretty s .<>. text ".." .<>. pretty s' .<>. text "]"
pretty (Let x s) = text "let" :--: emptyDoc .<>. varDefsToDoc x .<>. (emptyDoc :--: text "in" :--: emptyDoc) .<>. pretty s
pretty (Matrix _ s (v × v') s') = text "[" .<>. text "|" .<>. pretty s .<>. text "|" .<>. text "(" .<>. text v .<>. text "," .<>. text v' .<>. (text ")" :--: emptyDoc) .<>. (text "in" :--: emptyDoc) .<>. pretty s' .<>. text "|" .<>. text "]"
pretty (Constr _ "NonEmpty" _) = text "NonEmpty"
pretty (Constr _ "None" _) = text "None"
pretty (Constr _ "Pair" x) = text "(" .<>. listExpr x .<>. text ")"
pretty (Constr _ ":" x) = text "(" .<>. listExprList x .<>. text ")"
pretty (Constr _ c x) = (text c :--: emptyDoc) .<>. listExpr2 x
pretty (Dictionary _ x) = text "{" .<>. (text "|" :--: emptyDoc) .<>. dictToDoc x .<>. (emptyDoc :--: text "|") .<>. text "}"
pretty (Str _ x) = text "\"" .<>. text x .<>. text "\""
pretty (Float _ x) = text (show x)
pretty (ListComp _ s q) = text "[" .<>. pretty s .<>. text "|" .<>. qualifiersToDoc q .<>. text "]"
-- pretty (Matrix _ _ _ _) = text "[]"
-- pretty (Constr _ c x) = text "[]"
-- pretty _ = emptyDoc

listExprList :: forall a. List (Expr a) -> Doc 
listExprList (Cons x Nil) = pretty x 
listExprList (Cons x xs) = pretty x .<>. text ":" .<>. listExprList xs 
listExprList Nil = emptyDoc

listExpr2 :: forall a. List (Expr a) -> Doc
listExpr2 (Cons x Nil) = pretty x 
listExpr2 (Cons x xs) = (pretty x :--: emptyDoc) .<>. listExpr2 xs 
listExpr2 Nil = emptyDoc

listExpr :: forall a. List (Expr a) -> Doc 
listExpr (Cons x Nil) = pretty x 
listExpr (Cons x xs) = pretty x .<>. (text "," :--: emptyDoc) .<>. listExpr xs 
listExpr Nil = emptyDoc


qualifiersToDoc :: forall a. List (Qualifier a) -> Doc 
qualifiersToDoc (Cons (Guard s) Nil) = pretty s 
qualifiersToDoc (Cons (Declaration v) Nil) = (text "let" :--: emptyDoc) .<>. varDefToDoc v 
qualifiersToDoc (Cons (Generator p s) Nil) = prettyAuxillaryFuncPattern p .<>. text "<-" .<>. pretty s 
qualifiersToDoc (Cons (Guard s) xs) =  pretty s .<>. text "," .<>.  qualifiersToDoc xs 
qualifiersToDoc (Cons (Declaration v) xs) = (text "let" :--: emptyDoc) .<>. varDefToDoc v .<>. text "," .<>. qualifiersToDoc xs
qualifiersToDoc (Cons (Generator p s) xs) = prettyAuxillaryFuncPattern p .<>. text "<-" .<>. pretty s  .<>. text "," .<>. qualifiersToDoc xs 
qualifiersToDoc Nil = emptyDoc

letArg :: forall a. Expr a -> Doc 
letArg (Let x s) = text "(" .<>. pretty (Let x s) .<>. text ")"
letArg s = pretty s

dictToDoc :: forall a. List (Pair (Expr a)) -> Doc 
dictToDoc (Cons (Pair e e') Nil) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e'
dictToDoc (Cons (Pair e e') xs) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e' .<>. (text "," :--: emptyDoc) .<>. dictToDoc xs 
dictToDoc Nil = emptyDoc

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons x Nil) _ = x
intersperse' (Cons x xs) d = x .<>. d .-. intersperse' xs d
intersperse' Nil _ =  emptyDoc

varDefToDoc :: forall a. VarDef a -> Doc
varDefToDoc (VarDef p s) = (prettyAuxillaryFuncPattern p :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty s)

varDefsToDocSemiColon :: forall a. VarDefs a -> List Doc
varDefsToDocSemiColon x = toList (map varDefToDoc x)

temp :: List Doc -> Doc
temp x = intersperse' x (text ";")

varDefsToDoc :: forall a. VarDefs a -> Doc
varDefsToDoc x = intersperse' (toList (map varDefToDoc x)) (text ";")

-- varDefsToDoc :: forall a. VarDefs a -> Doc 
-- varDefsToDoc x = let docs = map varDefToDoc x in foldl (.-.) emptyDoc docs  

listAuxillaryFunc :: forall a. ListRest a -> Doc
listAuxillaryFunc (Next _ s x) = text "," .<>. text "" .<>. pretty s .<>. listAuxillaryFunc x
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
prettyAuxillaryFuncVarExpr (Cons x xs) = text (key x) .<>. text ":" .<>. pretty (val x) .-. prettyAuxillaryFuncVarExpr xs -- edited atop
prettyAuxillaryFuncVarExpr Nil = emptyDoc

prettyAuxillaryFuncListPattern :: ListRestPattern -> Doc
prettyAuxillaryFuncListPattern (PNext p x) = prettyAuxillaryFuncPattern p .<>. prettyAuxillaryFuncListPattern x
prettyAuxillaryFuncListPattern PEnd = emptyDoc

prettyAuxillaryFuncPattern :: Pattern -> Doc
prettyAuxillaryFuncPattern (PVar x) = text x
prettyAuxillaryFuncPattern (PRecord x) = text "{" .<>. prettyAuxillaryFuncVarPatt x .<>. text "}"
prettyAuxillaryFuncPattern (PConstr "Pair" x) = text "(" .<>. prettyAuxillaryFuncPatterns x true .<>. text ")"
prettyAuxillaryFuncPattern (PConstr "Empty" x) = text "Empty" .<>. prettyAuxillaryFuncPatterns x false
prettyAuxillaryFuncPattern (PConstr ":" x) = text "(" .<>. semiColonInfix x .<>. text ")"
prettyAuxillaryFuncPattern (PConstr c x) = text "(" .<>. (text c :--: emptyDoc) .<>. prettyAuxillaryFuncPatterns x false .<>. text ")"
-- prettyAuxillaryFuncPattern (PConstr c x) = text c .<>. text "(" .<>. prettyAuxillaryFuncPatterns x .<>. text ")"
prettyAuxillaryFuncPattern (PListEmpty) = text "[]"
prettyAuxillaryFuncPattern (PListNonEmpty p x) = text "["  .<>. prettyAuxillaryFuncPattern p .<>. listPatternToDoc x .<>. text "]"

semiColonInfix :: List Pattern -> Doc 
semiColonInfix (Cons x Nil) = prettyAuxillaryFuncPattern x
semiColonInfix (Cons x xs) = prettyAuxillaryFuncPattern x .<>. text ":" .<>. semiColonInfix xs 
semiColonInfix Nil = emptyDoc

listPatternToDoc :: ListRestPattern -> Doc 
listPatternToDoc (PNext p PEnd) = prettyAuxillaryFuncPattern p
listPatternToDoc (PNext p x) = text "," .<>. prettyAuxillaryFuncPattern p .<>. listPatternToDoc x
listPatternToDoc PEnd = emptyDoc
-- prettyAuxillaryFuncPattern _ = emptyDoc

prettyAuxillaryFuncVarPatt :: List (Bind (Pattern)) -> Doc
prettyAuxillaryFuncVarPatt (Cons x Nil) = text (key x) .<>. text ":" .<>. prettyAuxillaryFuncPattern (val x) .-. prettyAuxillaryFuncVarPatt Nil -- edited atop
prettyAuxillaryFuncVarPatt (Cons x xs) = text (key x) .<>. text ":" .<>. prettyAuxillaryFuncPattern (val x) .<>. text "," .-. prettyAuxillaryFuncVarPatt xs -- edited atop
prettyAuxillaryFuncVarPatt Nil = emptyDoc

prettyAuxillaryFuncPatterns :: List Pattern -> Boolean -> Doc
prettyAuxillaryFuncPatterns (Cons x Nil) true = prettyAuxillaryFuncPattern x
prettyAuxillaryFuncPatterns (Cons x xs) true = (prettyAuxillaryFuncPattern x .<>. text ",") .<>. prettyAuxillaryFuncPatterns xs true
prettyAuxillaryFuncPatterns (Cons x xs) false = (prettyAuxillaryFuncPattern x :--: emptyDoc) .<>. prettyAuxillaryFuncPatterns xs false -- beside may need to change to a space
prettyAuxillaryFuncPatterns Nil _ = emptyDoc

-- in Tex file Patt is more than one pattern but here it can be non-empty (might need to fix this)
-- prettyAuxillaryFuncPatterns :: List Pattern -> Doc
-- prettyAuxillaryFuncPatterns (Cons x xs)  = (prettyAuxillaryFuncPattern x :--: emptyDoc) .<>. prettyAuxillaryFuncPatterns xs -- beside may need to change to a space
-- prettyAuxillaryFuncPatterns Nil = emptyDoc

unitClauses :: forall a. Clause a -> Doc
unitClauses (Clause (ps × e)) = (prettyAuxillaryFuncPatterns (toList ps) false :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty e) -- edited beside with spaces

varClauses :: forall a. String -> Clause a -> Doc
varClauses x (Clause (ps × e)) = (text x :--: emptyDoc) .<>. unitClauses (Clause (ps × e))

prettyAuxillaryFuncClauses :: forall a. InFront -> Clauses a -> Doc
prettyAuxillaryFuncClauses Unit (Clauses cs) = intersperse' (toList (map unitClauses cs)) (text ";") -- beside may need to change (changed to space)
prettyAuxillaryFuncClauses (Prefix x) (Clauses cs) = intersperse' (toList (map (varClauses x) cs)) (text ";")



-- prettyAuxillaryFuncClauses (Prefix x) (Clauses cs) = let docs = map (varClauses x) cs in foldl (.-.) emptyDoc docs

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
auxillaryFunction3Rec x = intersperse' (toList (map auxillaryFunction31Rec x)) (text ";") 

combiningAuxillaryFunctionsRec :: forall a. RecDefs a -> Doc
combiningAuxillaryFunctionsRec x = auxillaryFunction3Rec (auxillaryFunction2Rec (auxillaryFunction1Rec x))

-- need to do normal beside on my documents 
-- and then apply beside2 to the normal beside 

-- for each clause have them as a list of documents 
-- then call intersperse' on this list 

-- dict_intersectionWith
--    (let x = 2 in (fun n m -> (n + m) * x))
--    {| "a" := 5, "b" := 6, "c" := 3 |}
--    {| "b" := -6, "c" := 7 |}