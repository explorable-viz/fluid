module Pretty3 (class Pretty, pretty) where

import Prelude hiding (absurd, between)

import Bindings (Bind, key, val)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList, groupBy, singleton, toList)
import Data.Map (keys)
import Data.Set (member)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), VarDef(..), VarDefs, Qualifier(..), RecDefs)
import Util ((×), type (×))
import Util.Pair (Pair(..))
import Util.Pretty (Doc, atop, beside, empty, space, text)

emptyDoc :: Doc
emptyDoc = empty 0 0

data InFront = Prefix (String) | Unit
type IsPair = Boolean × (List (Pattern))
newtype FirstGroup a = First (RecDefs a)
type IsMatch c = Boolean × (Clause c)
type IsConstrPair a = Boolean × (List (Expr a))

infixl 5 beside as .<>.
infixl 5 space as :--:
infixl 5 atop as .-.

class Pretty p where
   pretty :: p -> Doc

instance Pretty (Expr a) where
   pretty (Int _ n) = text (show n)
   pretty (App s s') = ((pretty s :--: emptyDoc) .<>. text "(" .<>. pretty s' .<>. text ")")
   pretty (Var x) = emptyDoc :--: text x :--: emptyDoc
   pretty (Op x) = text "(" .<>. text x .<>. text ")"
   pretty (BinaryApp s x s') = text "(" .<>. (pretty s :--: emptyDoc) .<>. checkOp x .<>. (emptyDoc :--: pretty s') .<>. text ")" -- edited
   pretty (IfElse s s_1 s_2) = (emptyDoc :--: text "if" :--: emptyDoc) .<>. pretty s .<>. (emptyDoc :--: text "then" :--: emptyDoc) .<>. pretty s_1 .<>. (emptyDoc :--: text "else" :--: emptyDoc) .<>. pretty s_2
   pretty (Project s x) = pretty s .<>. text "." .<>. text x
   pretty (Record _ x) = text "{" .<>. pretty x .<>. text "}"
   pretty (Lambda (Clauses cs)) = text "(" .<>. (text "fun" :--: emptyDoc) .<>. pretty (Clauses cs) .<>. text ")" :--: emptyDoc -- edited
   pretty (LetRec g s) = ((text "let" :--: emptyDoc .<>. pretty (First g)) .-. (emptyDoc :--: text "in" :--: emptyDoc)) .-. pretty s
   pretty (MatchAs s x) = (((text "match" :--: emptyDoc) .<>. text "(" .<>. pretty s .<>. text ")" .<>. (emptyDoc :--: text "as {")) .-. (emptyDoc :--: emptyDoc :--: emptyDoc :--: emptyDoc .<>. pretty x)) .-. text "}"
   pretty (ListEmpty _) = text "[]"
   pretty (ListNonEmpty _ s x) = emptyDoc :--: text "[" .<>. pretty s .<>. pretty x .<>. text "]"
   pretty (ListEnum s s') = text "[" .<>. pretty s .<>. text ".." .<>. pretty s' .<>. text "]"
   pretty (Let x s) = text "(" .<>. text "let" :--: emptyDoc .<>. pretty x .<>. (emptyDoc :--: text "in" :--: emptyDoc) .<>. pretty s .<>. text ")"
   pretty (Matrix _ s (v × v') s') = text "[" .<>. text "|" .<>. pretty s .<>. text "|" .<>. text "(" .<>. text v .<>. text "," .<>. text v' .<>. (text ")" :--: emptyDoc) .<>. (text "in" :--: emptyDoc) .<>. pretty s' .<>. text "|" .<>. text "]"
   pretty (Constr _ "Pair" x) = text "(" .<>. pretty (true × x) .<>. text ")"
   pretty (Constr _ ":" x) = text "(" .<>. pretty (false × x) .<>. text ")"
   pretty (Constr _ c x) = text "(" .<>. (text c :--: emptyDoc) .<>. pretty x .<>. text ")"
   pretty (Dictionary _ x) = text "{" .<>. (text "|" :--: emptyDoc) .<>. pretty x .<>. (emptyDoc :--: text "|") .<>. text "}"
   pretty (Str _ x) = text "\"" .<>. text x .<>. text "\""
   pretty (Float _ x) = text (show x)
   pretty (ListComp _ s q) = text "[" .<>. pretty s .<>. text "|" .<>. pretty q .<>. text "]"

instance Pretty (List (Bind (Expr a))) where
   pretty (Cons x Nil) = text (key x) .<>. text ":" .<>. pretty (val x)
   pretty (Cons x xs) = (text (key x) .<>. text ":" .<>. pretty (val x) .<>. text ",") .-. pretty xs -- edited atop
   pretty Nil = emptyDoc

instance Pretty (ListRest a) where
   pretty (Next _ s x) = text "," .<>. text "" .<>. pretty s .<>. pretty x
   pretty (End _) = emptyDoc

instance Pretty (List (Pair (Expr a))) where
   pretty (Cons (Pair e e') Nil) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e'
   pretty (Cons (Pair e e') xs) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e' .<>. (text "," :--: emptyDoc) .<>. pretty xs
   pretty Nil = emptyDoc

instance Pretty (Pattern) where
   pretty (PVar x) = text x
   pretty (PRecord x) = text "{" .<>. pretty x .<>. text "}"
   pretty (PConstr "Pair" x) = text "(" .<>. pretty (true × x) .<>. text ")"
   pretty (PConstr "Empty" x) = text "Empty" .<>. pretty (false × x)
   pretty (PConstr ":" x) = text "(" .<>. pretty x .<>. text ")"
   pretty (PConstr c x) = text "(" .<>. (text c :--: emptyDoc) .<>. pretty (false × x) .<>. text ")"
   pretty (PListEmpty) = text "[]"
   pretty (PListNonEmpty p x) = text "[" .<>. pretty p .<>. pretty x .<>. text "]"

instance Pretty (List (Bind (Pattern))) where
   pretty (Cons x Nil) = text (key x) .<>. text ":" .<>. pretty (val x) .-. emptyDoc
   pretty (Cons x xs) = text (key x) .<>. text ":" .<>. pretty (val x) .<>. text "," .-. pretty xs
   pretty Nil = emptyDoc

instance Pretty (IsPair) where
   pretty (_ × Nil) = emptyDoc
   pretty (_ × (Cons x Nil)) = pretty x
   pretty (true × (Cons x xs)) = (pretty x .<>. text ",") .<>. pretty (true × xs)
   pretty (false × (Cons x xs)) = (pretty x :--: emptyDoc) .<>. pretty (false × xs)

instance Pretty (List Pattern) where
   pretty (Cons x Nil) = pretty x
   pretty (Cons x xs) = pretty x .<>. text ":" .<>. pretty xs
   pretty Nil = emptyDoc

instance Pretty (ListRestPattern) where
   pretty (PNext p x) = text "," .<>. pretty p .<>. pretty x
   pretty PEnd = emptyDoc

instance Pretty (Boolean × Clause a) where
   pretty (true × Clause (ps × e)) = (pretty (false × toList (ps)) :--: emptyDoc) .<>. text "->" .<>. (emptyDoc :--: pretty e)
   pretty (false × Clause (ps × e)) = (pretty (false × (toList ps)) :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty e)

instance Pretty (Clauses a) where
   pretty (Clauses cs) = intersperse' (toList (map pretty (map (\x -> false × x) cs))) (text ";")

instance Pretty (Branch a) where
   pretty (x × Clause (ps × e)) = (text x :--: emptyDoc) .<>. pretty (false × Clause (ps × e))

instance Pretty (NonEmptyList (Branch a)) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Pretty (NonEmptyList (NonEmptyList (Branch a))) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Pretty (FirstGroup a) where
   pretty (First x) = pretty (groupBy (\p q -> key p == key q) x)

instance Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty x = intersperse' (map pretty (map helperMatch2 (map Clause (toList (helperMatch x))))) (text ";")

instance Pretty (VarDef a) where
   pretty (VarDef p s) = (pretty p :--: emptyDoc) .<>. text "=" .<>. (emptyDoc :--: pretty s)

instance Pretty (VarDefs a) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Pretty (IsConstrPair a) where
   pretty (_ × (Cons x Nil)) = pretty x
   pretty (true × (Cons x xs)) = pretty x .<>. (text "," :--: emptyDoc) .<>. pretty (true × xs)
   pretty (false × (Cons x xs)) = pretty x .<>. text ":" .<>. pretty (false × xs)
   pretty (_ × Nil) = emptyDoc

instance Pretty (List (Expr a)) where
   pretty (Cons x Nil) = pretty x
   pretty (Cons x xs) = (pretty x :--: emptyDoc) .<>. pretty xs
   pretty Nil = emptyDoc

instance Pretty (List (Qualifier a)) where
   pretty (Cons (Guard s) Nil) = pretty s
   pretty (Cons (Declaration v) Nil) = (text "let" :--: emptyDoc) .<>. pretty v
   pretty (Cons (Generator p s) Nil) = pretty p .<>. text "<-" .<>. pretty s
   pretty (Cons (Guard s) xs) = pretty s .<>. text "," .<>. pretty xs
   pretty (Cons (Declaration v) xs) = (text "let" :--: emptyDoc) .<>. pretty v .<>. text "," .<>. pretty xs
   pretty (Cons (Generator p s) xs) = pretty p .<>. text "<-" .<>. pretty s .<>. text "," .<>. pretty xs
   pretty Nil = emptyDoc

checkOp :: String -> Doc
checkOp x = case (member x (keys (opDefs))) of
   true -> text x
   false -> text "`" .<>. text x .<>. text "`"

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons x Nil) _ = x
intersperse' (Cons x xs) d = x .<>. d .-. intersperse' xs d
intersperse' Nil _ = emptyDoc

helperMatch :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
helperMatch x = map (\(a × b) -> singleton a × b) x

helperMatch2 :: forall a. Clause a -> Boolean × Clause a
helperMatch2 (Clause (ps × x)) = true × (Clause (ps × x))
