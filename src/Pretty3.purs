module Pretty3 (class Pretty, pretty) where 

import Prelude hiding (absurd, between)

import Bindings (Bind, key, val)
import Data.List (List(..))
import Data.Map (keys)
import Data.Set (member)
import Primitive.Parse (opDefs)
import SExpr (Expr(..), ListRest(..)) as S
import Util ((×))
import Util.Pair (Pair(..))
import Util.Pretty (Doc, atop, beside, empty, space, text)

-- import Bindings (Bind, Var, (↦), key, val)
-- import Data.Exists (runExists)
-- import Data.Foldable (class Foldable)
-- import Data.List (List(..), (:), fromFoldable, null)
-- import Data.List.NonEmpty (NonEmptyList)
-- import Data.List.NonEmpty (toList) as NEL
-- import Data.Map (keys)
-- import Data.Newtype (unwrap)
-- import Data.Profunctor.Choice ((|||))
-- import Data.Profunctor.Strong (first)
-- import Data.Set (member)
-- import Data.String (Pattern(..), contains) as Data.String
-- import DataType (Ctr, cCons, cNil, cPair, showCtr)
-- import Dict (Dict)
-- import Dict (toUnfoldable) as D
-- import Expr (Cont(..), Elim(..))
-- import Expr (Expr(..), VarDef(..)) as E
-- import Parse (str)
-- import Primitive.Parse (opDefs)
-- import SExpr (Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..)) as S
-- import Text.Pretty (render) as P
-- import Util (type (+), type (×), Endo, absurd, assert, error, intersperse, (×))
-- import Util.Pair (Pair(..))
-- import Util.Pair (toTuple)
-- import Util.Pretty (Doc, atop, beside, empty, hcat, render, space, text)
-- import Val (Fun(..), Val(..)) as V
-- import Val (class Highlightable, ForeignOp', Fun, Val, highlightIf)


emptyDoc :: Doc
emptyDoc = empty 0 0

data InFront = Prefix (String) | Unit


infixl 5 beside as .<>. 
infixl 5 space as :--:
infixl 5 atop as .-.


class Pretty p where 
    pretty :: p -> Doc 

instance Pretty (S.Expr a) where 
    pretty (S.Int _ n) = text (show n) 
    pretty (S.App s s') = ((pretty s :--: emptyDoc) .<>. text "(" .<>. pretty s' .<>. text ")")
    pretty (S.Var x) = emptyDoc :--: text x :--: emptyDoc 
    pretty (S.Op x) = text "(" .<>. text x .<>. text ")"
    pretty (S.BinaryApp s x s') = text "(" .<>. (pretty s :--: emptyDoc) .<>. checkOp x .<>. (emptyDoc :--: pretty s') .<>. text ")" -- edited
    pretty (S.IfElse s s_1 s_2) = (emptyDoc :--: text "if" :--: emptyDoc) .<>. pretty s .<>. (emptyDoc :--: text "then" :--: emptyDoc) .<>. pretty s_1 .<>. (emptyDoc :--: text "else" :--: emptyDoc) .<>. pretty s_2
    pretty (S.Project s x) = pretty s .<>. text "." .<>. text x
    pretty (S.Record _ x) = text "{" .<>. pretty x .<>. text "}" 
    --pretty (S.Lambda S.Clauses cs) = text "(" .<>. (text "fun" :--: emptyDoc) .<>. pretty Unit (S.Clauses cs) .<>. text ")" :--: emptyDoc -- edited
    --pretty (S.LetRec g s) = ((text "let" :--: emptyDoc .<>. recDefsToDoc g) .-. (emptyDoc :--: text "in" :--: emptyDoc)) .-. pretty s
    --pretty (S.MatchAs s x) = (((text "match" :--: emptyDoc) .<>. text "(" .<>. pretty s .<>. text ")" .<>. (emptyDoc :--: text "as {")) .-. (emptyDoc :--: emptyDoc :--: emptyDoc :--: emptyDoc .<>. matchToDoc x)) .-. text "}"
    pretty (S.ListEmpty _) = text "[]"
    pretty (S.ListNonEmpty _ s x) = emptyDoc :--: text "[" .<>. pretty s .<>. pretty x .<>. text "]" 
    pretty (S.ListEnum s s') = text "[" .<>. pretty s .<>. text ".." .<>. pretty s' .<>. text "]"
    --pretty (S.Let x s) = text "(" .<>. text "let" :--: emptyDoc .<>. varDefsToDoc x .<>. (emptyDoc :--: text "in" :--: emptyDoc) .<>. pretty s .<>. text ")"
    pretty (S.Matrix _ s (v × v') s') = text "[" .<>. text "|" .<>. pretty s .<>. text "|" .<>. text "(" .<>. text v .<>. text "," .<>. text v' .<>. (text ")" :--: emptyDoc) .<>. (text "in" :--: emptyDoc) .<>. pretty s' .<>. text "|" .<>. text "]"
    --pretty (S.Constr _ "NonEmpty" x) = (text "(NonEmpty" :--: emptyDoc) .<>. listExpr x .<>. text ")"
    --pretty (S.Constr _ "None" _) = text "None"
    --pretty (S.Constr _ "Empty" _) = text "Empty"
    --pretty (S.Constr _ "Pair" x) = text "(" .<>. listExprPair x .<>. text ")"
    --pretty (S.Constr _ ":" x) = text "(" .<>. listExprList x .<>. text ")"
    --pretty (S.Constr _ c x) = text "(" .<>. (text c :--: emptyDoc) .<>. listExpr x .<>. text ")"
    pretty (S.Dictionary _ x) = text "{" .<>. (text "|" :--: emptyDoc) .<>. pretty x .<>. (emptyDoc :--: text "|") .<>. text "}"
    pretty (S.Str _ x) = text "\"" .<>. text x .<>. text "\""
    pretty (S.Float _ x) = text (show x)
    --pretty (S.ListComp _ s q) = text "[" .<>. pretty s .<>. text "|" .<>. qualifiersToDoc q .<>. text "]"
    pretty _ = emptyDoc 

instance Pretty (List (Bind (S.Expr a))) where 
    pretty (Cons x Nil) = text (key x) .<>. text ":" .<>. pretty (val x)
    pretty (Cons x xs) = (text (key x) .<>. text ":" .<>. pretty (val x) .<>. text ",") .-. pretty xs -- edited atop
    pretty Nil = emptyDoc

instance Pretty (S.ListRest a) where 
    pretty (S.Next _ s x) = text "," .<>. text "" .<>. pretty s .<>. pretty x
    pretty (S.End _) = emptyDoc

instance Pretty (List (Pair (S.Expr a))) where 
    pretty (Cons (Pair e e') Nil) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e'
    pretty (Cons (Pair e e') xs) = pretty e .<>. (emptyDoc :--: text ":=" :--: emptyDoc) .<>. pretty e' .<>. (text "," :--: emptyDoc) .<>. pretty xs
    pretty Nil = emptyDoc

checkOp :: String -> Doc
checkOp x = case (member x (keys (opDefs))) of
   true -> text x
   false -> text "`" .<>. text x .<>. text "`"
