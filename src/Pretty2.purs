module Pretty2 where

import Prelude

import Bindings (Bind, key, val)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList, toList, groupBy, head, singleton)
import Data.Map (keys)
import Data.Set (member)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), RecDefs, VarDef(..), VarDefs, Qualifier(..))
import Util ((×), type (×))
import Util.Pair (Pair(..))
import Util.Pretty (Doc, atop, beside,  empty, space, text)

infixl 5 atop as .-.
infixl 5 beside as .<>.
data InFront = Prefix (String) | Unit

infixl 5 space as :--:

emptyDoc :: Doc
emptyDoc = empty 0 0

pretty :: forall a. Expr a -> Doc
pretty (Int _ n) = text (show n) 
pretty (App s s') = ((pretty s :--: emptyDoc) .<>. text "(" .<>. pretty s' .<>. text ")")
pretty (Var x) = emptyDoc :--: text x :--: emptyDoc 
pretty (Op x) = text "(" .<>. text x .<>. text ")"
pretty (BinaryApp s x s') = text "(" .<>. (pretty s :--: emptyDoc) .<>. checkOp x .<>. (emptyDoc :--: pretty s') .<>. text ")" -- edited
pretty (IfElse s s_1 s_2) = (emptyDoc :--: text "if" :--: emptyDoc) .<>. pretty s .<>. (emptyDoc :--: text "then" :--: emptyDoc) .<>. pretty s_1 .<>. (emptyDoc :--: text "else" :--: emptyDoc) .<>. pretty s_2
pretty (Project s x) = pretty s .<>. text "." .<>. text x
pretty (Record _ x) = text "{" .<>. varExprToDoc x .<>. text "}" -- formatting needs fixing 
pretty (Lambda (Clauses cs)) = text "(" .<>. (text "fun" :--: emptyDoc) .<>. clausesToDoc Unit (Clauses cs) .<>. text ")" :--: emptyDoc -- edited
pretty (LetRec g s) = ((text "let" :--: emptyDoc .<>. recDefsToDoc g) .-. (emptyDoc :--: text "in" :--: emptyDoc)) .-. pretty s
pretty (MatchAs s x) = (((text "match" :--: emptyDoc) .<>. text "(" .<>. pretty s .<>. text ")" .<>. (emptyDoc :--: text "as {")) .-. (emptyDoc :--: emptyDoc :--: emptyDoc :--: emptyDoc .<>. matchToDoc x)) .-. text "}"
pretty (ListEmpty _) = text "[]"
pretty (ListNonEmpty _ s x) = emptyDoc :--: text "[" .<>. pretty s .<>. listRestToDoc x .<>. text "]" -- edited
pretty (ListEnum s s') = text "[" .<>. pretty s .<>. text ".." .<>. pretty s' .<>. text "]"
pretty (Let x s) = text "(" .<>. text "let" :--: emptyDoc .<>. varDefsToDoc x .<>. (emptyDoc :--: text "in" :--: emptyDoc) .<>. pretty s .<>. text ")"
pretty (Matrix _ s (v × v') s') = text "[" .<>. text "|" .<>. pretty s .<>. text "|" .<>. text "(" .<>. text v .<>. text "," .<>. text v' .<>. (text ")" :--: emptyDoc) .<>. (text "in" :--: emptyDoc) .<>. pretty s' .<>. text "|" .<>. text "]"
pretty (Constr _ "NonEmpty" x) = (text "(NonEmpty" :--: emptyDoc) .<>. listExpr x .<>. text ")"
pretty (Constr _ "None" _) = text "None"
pretty (Constr _ "Empty" _) = text "Empty"
pretty (Constr _ "Pair" x) = text "(" .<>. listExprPair x .<>. text ")"
pretty (Constr _ ":" x) = text "(" .<>. listExprList x .<>. text ")"
pretty (Constr _ c x) = text "(" .<>. (text c :--: emptyDoc) .<>. listExpr x .<>. text ")"
pretty (Dictionary _ x) = text "{" .<>. (text "|" :--: emptyDoc) .<>. dictToDoc x .<>. (emptyDoc :--: text "|") .<>. text "}"
pretty (Str _ x) = text "\"" .<>. text x .<>. text "\""
pretty (Float _ x) = text (show x)
pretty (ListComp _ s q) = text "[" .<>. pretty s .<>. text "|" .<>. qualifiersToDoc q .<>. text "]"

qualifiersToDoc :: forall a. List (Qualifier a) -> Doc
qualifiersToDoc (Cons (Guard s) Nil) = pretty s
qualifiersToDoc (Cons (Declaration v) Nil) = (text "let" :--: emptyDoc) .<>. varDefToDoc v
qualifiersToDoc (Cons (Generator p s) Nil) = patternToDoc p .<>. text "<-" .<>. pretty s
qualifiersToDoc (Cons (Guard s) xs) = pretty s .<>. text "," .<>. qualifiersToDoc xs
qualifiersToDoc (Cons (Declaration v) xs) = (text "let" :--: emptyDoc) .<>. varDefToDoc v .<>. text "," .<>. qualifiersToDoc xs
qualifiersToDoc (Cons (Generator p s) xs) = patternToDoc p .<>. text "<-" .<>. pretty s .<>. text "," .<>. qualifiersToDoc xs
qualifiersToDoc Nil = emptyDoc

dictToDoc :: forall a. List (Pair (Expr a)) -> Doc
dictToDoc (Cons (Pair e e') Nil) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e'
dictToDoc (Cons (Pair e e') xs) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e' .<>. (text "," :--: emptyDoc) .<>. dictToDoc xs
dictToDoc Nil = emptyDoc

varDefToDoc :: forall a. VarDef a -> Doc
varDefToDoc (VarDef p s) = (patternToDoc p :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty s)

varDefsToDoc :: forall a. VarDefs a -> Doc
varDefsToDoc x = intersperse' (toList (map varDefToDoc x)) (text ";")

listRestToDoc :: forall a. ListRest a -> Doc
listRestToDoc (Next _ s x) = text "," .<>. text "" .<>. pretty s .<>. listRestToDoc x
listRestToDoc (End _) = emptyDoc

matchToDoc :: forall a. NonEmptyList (Pattern × Expr a) -> Doc
matchToDoc x = matchClauses (Clauses (map Clause (helperMatch1 x)))

varExprToDoc :: forall a. List (Bind (Expr a)) -> Doc
varExprToDoc (Cons x Nil) = text (key x) .<>. text ":" .<>. pretty (val x)
varExprToDoc (Cons x xs) = (text (key x) .<>. text ":" .<>. pretty (val x) .<>. text ",") .-. varExprToDoc xs -- edited atop
varExprToDoc Nil = emptyDoc

listRestPatternToDoc :: ListRestPattern -> Doc
listRestPatternToDoc (PNext p x) = patternToDoc p .<>. listRestPatternToDoc x
listRestPatternToDoc PEnd = emptyDoc

patternToDoc :: Pattern -> Doc
patternToDoc (PVar x) = text x
patternToDoc (PRecord x) = text "{" .<>. varPattToDoc x .<>. text "}"
patternToDoc (PConstr "Pair" x) = text "(" .<>. pairPattToDoc x true .<>. text ")"
patternToDoc (PConstr "Empty" x) = text "Empty" .<>. pairPattToDoc x false
patternToDoc (PConstr ":" x) = text "(" .<>. listPattToDoc x .<>. text ")"
patternToDoc (PConstr c Nil) = (text c :--: emptyDoc)
patternToDoc (PConstr c x) = text "(" .<>. (text c :--: emptyDoc) .<>. pairPattToDoc x false .<>. text ")"
patternToDoc (PListEmpty) = text "[]"
patternToDoc (PListNonEmpty p x) = text "[" .<>. patternToDoc p .<>. listlistRestPatternToDoc x .<>. text "]"

listlistRestPatternToDoc :: ListRestPattern -> Doc
listlistRestPatternToDoc (PNext p x) = text "," .<>. patternToDoc p .<>. listlistRestPatternToDoc x
listlistRestPatternToDoc PEnd = emptyDoc

varPattToDoc :: List (Bind (Pattern)) -> Doc
varPattToDoc (Cons x Nil) = text (key x) .<>. text ":" .<>. patternToDoc (val x) .-. varPattToDoc Nil -- edited atop
varPattToDoc (Cons x xs) = text (key x) .<>. text ":" .<>. patternToDoc (val x) .<>. text "," .-. varPattToDoc xs -- edited atop
varPattToDoc Nil = emptyDoc

pairPattToDoc :: List Pattern -> Boolean -> Doc
pairPattToDoc (Cons x Nil) true = patternToDoc x
pairPattToDoc (Cons x xs) true = (patternToDoc x .<>. text ",") .<>. pairPattToDoc xs true
pairPattToDoc (Cons x xs) false = (patternToDoc x :--: emptyDoc) .<>. pairPattToDoc xs false -- beside may need to change to a space
pairPattToDoc Nil _ = emptyDoc

unitClauses :: forall a. Clause a -> Doc
unitClauses (Clause (ps × e)) = (pairPattToDoc (toList ps) false :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty e) -- edited beside with spaces

varClauses :: forall a. String -> Clause a -> Doc
varClauses x (Clause (ps × e)) = (text x :--: emptyDoc) .<>. unitClauses (Clause (ps × e))

clausesToDoc :: forall a. InFront -> Clauses a -> Doc
clausesToDoc Unit (Clauses cs) = intersperse' (toList (map unitClauses cs)) (text ";")
clausesToDoc (Prefix x) (Clauses cs) = intersperse' (toList (map (varClauses x) cs)) (text ";")

recDefsToDoc :: forall a. RecDefs a -> Doc
recDefsToDoc x = helperRecDefs3 (helperRecDefs2 (helperRecDefs1 x))

checkOp :: String -> Doc
checkOp x = case (member x (keys (opDefs))) of
   true -> text x
   false -> text "`" .<>. text x .<>. text "`"

listExprList :: forall a. List (Expr a) -> Doc
listExprList (Cons x Nil) = pretty x
listExprList (Cons x xs) = pretty x .<>. text ":" .<>. listExprList xs
listExprList Nil = emptyDoc

listExpr :: forall a. List (Expr a) -> Doc
listExpr (Cons x Nil) = pretty x
listExpr (Cons x xs) = (pretty x :--: emptyDoc) .<>. listExpr xs
listExpr Nil = emptyDoc

listExprPair :: forall a. List (Expr a) -> Doc
listExprPair (Cons x Nil) = pretty x
listExprPair (Cons x xs) = pretty x .<>. (text "," :--: emptyDoc) .<>. listExprPair xs
listExprPair Nil = emptyDoc

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons x Nil) _ = x
intersperse' (Cons x xs) d = x .<>. d .-. intersperse' xs d
intersperse' Nil _ = emptyDoc

listPattToDoc :: List Pattern -> Doc
listPattToDoc (Cons x Nil) = patternToDoc x
listPattToDoc (Cons x xs) = patternToDoc x .<>. text ":" .<>. listPattToDoc xs
listPattToDoc Nil = emptyDoc

helperMatch1 :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
helperMatch1 x = map helperMatch11 x

helperMatch11 :: forall a. Pattern × Expr a -> NonEmptyList Pattern × Expr a
helperMatch11 (a × b) = singleton a × b

helperMatch2 :: forall a. NonEmptyList (NonEmptyList Pattern × Expr a) -> NonEmptyList (Clause a)
helperMatch2 x = map (\y -> Clause y) x

helperMatch3 :: forall a. NonEmptyList (Clause a) -> Clauses a
helperMatch3 x = Clauses x

matchClause :: forall a. Clause a -> Doc
matchClause (Clause (ps × e)) = (pairPattToDoc (toList ps) false :--: emptyDoc) .<>. text "->" .<>. (emptyDoc :--: pretty e) -- edited beside with spaces

matchClauses :: forall a. Clauses a -> Doc
matchClauses (Clauses cs) = intersperse' (toList (map matchClause cs)) (text ";")

helperRecDefs1 :: forall a. RecDefs a -> NonEmptyList (NonEmptyList (Branch a))
helperRecDefs1 x = groupBy (\p q -> key p == key q) x

helperRecDefs21 :: forall a. NonEmptyList (Branch a) -> String × NonEmptyList (Clause a)
helperRecDefs21 x = key (head x) × map (\y -> val y) x

helperRecDefs2 :: forall a. NonEmptyList (NonEmptyList (Branch a)) -> NonEmptyList (String × NonEmptyList (Clause a))
helperRecDefs2 x = map helperRecDefs21 x

helperRecDefs31 :: forall a. String × NonEmptyList (Clause a) -> Doc
helperRecDefs31 (a × b) = clausesToDoc (Prefix a) (Clauses b)

helperRecDefs3 :: forall a. NonEmptyList (String × NonEmptyList (Clause a)) -> Doc
helperRecDefs3 x = intersperse' (toList (map helperRecDefs31 x)) (text ";")