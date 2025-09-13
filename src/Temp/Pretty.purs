module Temp.Pretty (PrettyShow(..), class Pretty, compare, pretty, prettyPy) where

import Prelude

import Bind (Bind, Var)
import Data.List (List(..), fromFoldable, singleton, (:))
import Data.List.NonEmpty (NonEmptyList, head, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable)
import DataType (Ctr, cCons)
import Dict (Dict)
import Expr (Cont(..), Elim(..))
import Expr as E
import Graph (Vertex(..))
import Lattice (class BotOf, class MeetSemilattice, class Neg, botOf, symmetricDiff)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), ParagraphElem(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Temp.Pretty.Constants (_case, _colon, _comma, _def, _ellipsis, _else, _empty, _for, _if, _in, _lambda, _match)
import Temp.Pretty.Doc (Doc, array, block, record, render, text, (<+++>), (<++>), (<+>))
import Temp.Pretty.Helpers (braces, brackets, hsep, matrix, number, pair, parens, string, vsep)
import Util (type (×), Endo, (×))
import Util.Map (toUnfoldable)
import Util.Pair (Pair(..))
import Val (class Ann)

class Pretty p where
   pretty :: p -> Doc

newtype PrettyShow a = PrettyShow a

derive instance Newtype (PrettyShow a) _

instance Pretty a => Show (PrettyShow a) where
   show (PrettyShow x) = pretty x # render

instance Pretty String where
   pretty = text

prettyPy :: forall a. Pretty a => a -> String
prettyPy x = render (pretty x)

binaryApp :: forall a. Ann a => Int -> Expr a -> Doc
binaryApp n (BinaryApp s op s') =
   case getPrec op of
      -1 -> binaryApp 0 s <+> text "|" <> text op <> text "|" <+> binaryApp 0 s'
      n' ->
         if n' <= n then
            parens (binaryApp n' s <+> text op <+> binaryApp n' s')
         else
            binaryApp n' s <+> text op <+> binaryApp n' s'
   where
   getPrec :: String -> Int
   getPrec x = case lookup x opDefs of
      Just y -> y.prec
      Nothing -> -1
binaryApp _ e@(Constr _ c _) | c == cCons = parens (pretty e)
binaryApp _ (Let _ _) = text "undefined"
binaryApp _ (LetRec _ _) = text "undefined"
binaryApp _ e = pretty e

lambda :: forall a. Ann a => List Pattern -> Expr a -> Doc
lambda ps e = _lambda <+> prettyList ps <> _colon <+> pretty e

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = text x
   pretty (Op o) = parens $ text o
   pretty (Int _ n) = number n
   pretty (Float _ n) = number n
   pretty (Str _ str) = string str
   pretty (Constr _ c Nil) = text c
   pretty (Constr _ c as) = prettyConstr c as
   pretty (Dictionary _ Nil) = text "{}"
   pretty (Dictionary _ es) = record $ map pretty es
   pretty (Matrix _ e (x × y) e') = matrix (pretty e <+> _for <+> pair text x y <+> _in <+> pretty e')
   pretty (Lambda cs) = parens (pretty cs)
   pretty (Project s x) = pretty s <> text "." <> text x
   pretty (DProject e k) = pretty e <> brackets (pretty k)
   pretty (App s s') = prettyAppChain (App s s') Nil
   pretty (BinaryApp s op s') = binaryApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = _match <+> pretty s <> block (pretty cs)
   pretty (IfElse i t e) = _if <+> pretty i <> block (pretty t) <++> _else <> block (pretty e)
   pretty (ListEmpty _) = _empty
   pretty (ListNonEmpty _ e rest) = array $ (pretty e : collect rest)
      where
      collect :: ListRest a -> List Doc
      collect (Next _ e' rest') = pretty e' : collect rest'
      collect (End _) = Nil

   pretty (ListEnum s s') = brackets (pretty s <+> _ellipsis <+> pretty s')
   pretty (ListComp _ s qs) = brackets (pretty s <+> pretty qs)
   pretty (Let ds s) = (pretty ds) <> text ";" <+++> pretty s
   pretty (LetRec h s) = (pretty h) <> text ";" <+++> pretty s
   pretty (Paragraph p) = pretty p
   pretty (DocExpr p e) = text "@doc" <> parens (pretty p) <+> pretty e

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (ListCompDecl (VarDef v s)) Nil) = _for <+> pretty v <+> _in <+> brackets (pretty s)
   pretty (Cons (ListCompGuard s) Nil) = _if <+> pretty s
   pretty (Cons (ListCompGen p s) Nil) = _for <+> pretty p <+> _in <+> pretty s
   pretty (Cons q qs) = pretty (singleton q) <+> pretty qs
   pretty Nil = mempty

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty cs = vsep (toList (pretty <$> cs))

instance Ann a => Pretty (Pattern × Expr a) where
   pretty (p × e) = _case <+> (pretty p) <> block (pretty e)

instance Pretty Pattern where
   pretty (PVar x) = text x
   pretty (PRecord xps) = record $ map pretty xps
   pretty (PConstr c ps) = prettyConstr c ps
   pretty (PListEmpty) = _empty
   pretty (PListNonEmpty p l) = brackets (pretty p <> pretty l)

instance Pretty (String × Pattern) where
   pretty (k × v) = text k <> _colon <+> pretty v

instance Pretty ListRestPattern where
   pretty (PListVar x) = text x
   pretty (PListNext p l) = _comma <+> pretty p <> pretty l
   pretty PListEnd = mempty

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef v s) = _def <+> pretty v <> block (pretty s)

instance Ann a => Pretty (VarDefs a) where
   pretty ds = vsep (toList (pretty <$> ds))

instance Ann a => Pretty (Clause a) where
   pretty (Clause (ps × e)) = lambda (toList ps) e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) = pretty (head cs) -- TODO: head ?

instance Ann a => Pretty (RecDefs a) where
   pretty bs = vsep (toList (pretty <$> bs))

instance Ann a => Pretty (Branch a) where
   pretty (v × Clause (ps × e)) =
      _def
         <+> text v
         <> parens (prettyList (toList ps))
         <> block (pretty e)

instance Ann a => Pretty (DictEntry a × Expr a) where
   pretty (k × v) = pretty k <> _colon <+> pretty v

instance Ann a => Pretty (DictEntry a) where
   pretty (ExprKey k) = brackets (pretty k)
   pretty (VarKey _ k) = text k

instance Ann a => Pretty (List (ParagraphElem a)) where
   pretty xs = text "\"\"\"" <> hsep (pretty <$> xs) <> text "\"\"\""

instance Ann a => Pretty (ParagraphElem a) where
   pretty (Token str) = text str
   pretty (Unquote e) = text "${" <> pretty e <> text "}"

prettyConstr :: forall d. Pretty d => Ctr -> List d -> Doc
prettyConstr c Nil = text c
prettyConstr "Pair" (x : y : Nil) = pair pretty x y
prettyConstr ":" (x : y : Nil) = pretty x <+> text ":|" <+> pretty y
prettyConstr c ps = text c <> parens (prettyList ps)

prettyAppChain :: forall a. Ann a => Expr a -> List (Expr a) -> Doc
prettyAppChain (App f a) as = prettyAppChain f (a : as)
prettyAppChain f as = pretty f <> parens (prettyList as)

commas :: List Doc -> Doc
commas Nil = mempty
commas (d : Nil) = d
commas (d : ds) = d <> _comma <+> commas ds

prettyList :: forall f a. Foldable f => Pretty a => f a -> Doc
prettyList xs = commas (pretty <$> fromFoldable xs)

class Highlightable a where
   highlightIf :: a -> Endo Doc

instance Highlightable Unit where
   highlightIf _ = identity

instance Highlightable Boolean where
   highlightIf false = identity
   highlightIf true = \doc -> text "⸨" <> doc <> text "⸩"

instance Highlightable Vertex where
   highlightIf (Vertex α) = \doc -> doc <> text "_" <> text ("⟨" <> α <> "⟩")

instance Highlightable a => Pretty (Pair (E.Expr a)) where
   pretty (Pair k v) = pretty k <> text ":" <+> pretty v

instance Highlightable a => Pretty (E.Expr a) where
   pretty (E.Var x) = text x
   pretty (E.Op op) = parens (text op)
   pretty (E.Int a n) = highlightIf a (number n)
   pretty (E.Float a n) = highlightIf a (number n)
   pretty (E.Str a str) = highlightIf a (string str)
   pretty (E.Dictionary a ees) = highlightIf a $ record (pretty <$> ees)
   pretty (E.Constr a c es) = highlightIf a (prettyConstr c es)
   pretty (E.Matrix a e1 (i × j) e2) = highlightIf a $ matrix (pretty e1 <+> _for <+> pair text i j <+> _in <+> pretty e2)
   pretty (E.Lambda a o) = highlightIf a (text "lambda") <+> pretty o -- really?
   pretty (E.Project e x) = pretty e <> text "." <> pretty x
   pretty (E.DProject e x) = pretty e <> brackets (pretty x)
   pretty (E.App e e') = pretty e <> parens (pretty e') -- TODO
   pretty (E.Let (E.VarDef o e) e') = text "def" <+> pretty o <> text ":" <> block (pretty e) <+++> pretty e'
   pretty (E.LetRec (E.RecDefs _ p) e') = text "def" <+> pretty p <+++> pretty e'
   pretty (E.DocExpr p e) = text "@doc" <> parens (pretty p) <+> pretty e

instance Highlightable a => Pretty (Cont a) where
   pretty (ContExpr e) = pretty e
   pretty (ContElim σ) = pretty σ

instance Highlightable a => Pretty (Elim a) where
   pretty (ElimVar x k) = pretty x <> text "->" <> pretty k
   pretty (ElimConstr ks) = prettyList ks
   pretty (ElimDict xs k) = braces (prettyList xs) <+> text "->" <+> braces (pretty k)

instance Highlightable a => Pretty (Dict (Elim a)) where
   pretty ρ = go (toUnfoldable ρ)
      where
      go :: List (Var × Elim a) -> Doc
      go Nil = mempty
      go (xσ : Nil) = pretty xσ
      go (xσ : δ) = (go δ <+> text ";") <+> (pretty xσ)

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty _ = text ""

compare :: forall a. BotOf a a => Neg a => MeetSemilattice a => Eq a => Pretty a => String -> String -> a -> a -> String × String
compare op1 op2 x y =
   let
      x_minus_y × y_minus_x = symmetricDiff x y
      left = if x_minus_y == botOf x then "" else op1 <> " but not " <> op2 <> ":\n" <> prettyPy x_minus_y
      right = if y_minus_x == botOf x then "" else op2 <> " but not " <> op1 <> ":\n" <> prettyPy y_minus_x
   in
      left × right
