module Temp.Pretty (PrettyShow(..), class Pretty, class Ann, class Highlightable, compare, highlightIf, pretty, prettyP) where

import Prelude

import Bind (Bind, Var, (↦))
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
import Lattice (class BotOf, class BoundedLattice, class MeetSemilattice, class Neg, botOf, symmetricDiff)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), ParagraphElem(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Temp.Pretty.Constants (_case, _colon, _comma, _def, _ellipsis, _else, _empty, _for, _if, _in, _lambda, _match)
import Temp.Pretty.Doc (Doc, block, record, render, text, (<+++>), (<++>), (<+>))
import Temp.Pretty.Helpers (braces, brackets, hsep, matrix, number, pair, parens, string, vsep)
import Util (type (×), Endo, (×))
import Util.Map (toUnfoldable)
import Util.Pair (Pair(..))
import Val (BaseVal(..), Fun(..)) as V
import Val (BaseVal, DictRep(..), Env(..), EnvExpr(..), ForeignOp(..), Fun, MatrixRep(..), Val(..))

class Pretty p where
   pretty :: p -> Doc

newtype PrettyShow a = PrettyShow a

derive instance Newtype (PrettyShow a) _

instance Pretty a => Show (PrettyShow a) where
   show (PrettyShow x) = pretty x # render

instance Pretty String where
   pretty = text

class RootOp (e :: Type) where
   rootOp :: e -> Maybe String

instance RootOp Pattern where
   rootOp (PConstr c _) | c == cCons = Just ":"
   rootOp _ = Nothing

instance Ann a => RootOp (Expr a) where
   rootOp (Constr _ c _) | c == cCons = Just ":"
   rootOp (BinaryApp _ op _) = Just op
   rootOp _ = Nothing

instance Highlightable a => RootOp (E.Expr a) where
   rootOp (E.Constr _ c _) | c == cCons = Just ":"
   rootOp _ = Nothing

instance Highlightable a => RootOp (Val a) where
   rootOp (Val _ Nothing u) = rootOp u
   rootOp (Val _ (Just _) _) = Nothing

instance Highlightable a => RootOp (BaseVal a) where
   rootOp (V.Constr c _) | c == cCons = Just ":"
   rootOp _ = Nothing

prettyP :: forall a. Pretty a => a -> String
prettyP x = render (pretty x)

getPrec :: String -> Int
getPrec x = case lookup x opDefs of
   Just y -> y.prec
   Nothing -> -1

binaryApp :: forall a. Ann a => Int -> Expr a -> Doc
binaryApp n (BinaryApp s op s') =
   case getPrec op of
      -1 -> binaryApp 0 s <+> text "|" <> text op <> text "|" <+> binaryApp 0 s'
      n' ->
         if n' <= n then
            parens (binaryApp n' s <+> text op <+> binaryApp n' s')
         else
            binaryApp n' s <+> text op <+> binaryApp n' s'
binaryApp _ e@(Constr _ c _) | c == cCons = parens (pretty e)
binaryApp _ (Let _ _) = text "undefined"
binaryApp _ (LetRec _ _) = text "undefined"
binaryApp _ e = pretty e

lambda :: forall a. Ann a => List Pattern -> Expr a -> Doc
lambda ps e = _lambda <+> prettyList ps <> _colon <+> pretty e

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = text x
   pretty (Op o) = parens $ text o
   pretty (Int α n) = highlightIf α (number n)
   pretty (Float α n) = highlightIf α (number n)
   pretty (Str α str) = highlightIf α (string str)
   pretty (Constr α c Nil) = highlightIf α (text c)
   pretty (Constr α c as) = highlightIf α (prettyConstr c as)
   pretty (Dictionary α es) = highlightIf α (record $ map pretty es)
   pretty (Matrix α e (x × y) e') = highlightIf α (matrix (pretty e <+> _for <+> pair text x y <+> _in <+> pretty e'))
   pretty (Lambda cs) = parens (pretty cs)
   pretty (Project s x) = pretty s <> text "." <> text x
   pretty (DProject e k) = pretty e <> brackets (pretty k)
   pretty (App s s') = prettyAppChain (App s s') Nil
   pretty (BinaryApp s op s') = binaryApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = _match <+> pretty s <> block (pretty cs)
   pretty (IfElse i t e) = _if <+> pretty i <> block (pretty t) <++> _else <> block (pretty e)
   pretty (ListEmpty α) = highlightIf α _empty
   -- TODO: list dictionary case??
   pretty (ListNonEmpty α e rest) = highlightIf α (text "[") <> pretty e <> collect rest
      where
      collect :: ListRest a -> Doc
      collect (Next α' e' rest') = highlightIf α' (text ",") <+> pretty e' <> collect rest'
      collect (End α') = highlightIf α' (text "]")

   pretty (ListEnum s s') = brackets (pretty s <+> _ellipsis <+> pretty s')
   pretty (ListComp α s qs) = highlightIf α (brackets (pretty s <+> pretty qs))
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
   pretty (PConstr c Nil) = text c
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

prettyConstr :: forall a. RootOp a => Pretty a => Ctr -> List a -> Doc
prettyConstr "Nil" Nil = _empty
prettyConstr "Pair" (x : y : Nil) = pair pretty x y
prettyConstr ":" (x : y : Nil) = prettyConsArg x true <+> text ":|" <+> prettyConsArg y false
prettyConstr c Nil = text c
prettyConstr c ps = text c <> parens (prettyList ps)

prettyConsArg :: forall a. RootOp a => Pretty a => a -> Boolean -> Doc
prettyConsArg e lhs = case rootOp e of
   Nothing -> pretty e
   Just op -> if (if lhs then (<=) else (<)) (getPrec op) (getPrec ":") then parens (pretty e) else pretty e

prettyAppChain :: forall a. Ann a => Expr a -> List (Expr a) -> Doc
prettyAppChain (App f a) as = prettyAppChain f (a : as)
prettyAppChain f as = pretty f <> parens (prettyList as)

commas :: List Doc -> Doc
commas Nil = mempty
commas (d : Nil) = d
commas (d : ds) = d <> _comma <+> commas ds

vcommas :: List Doc -> Doc
vcommas Nil = mempty
vcommas (d : Nil) = d
vcommas (d : ds) = d <> _comma <++> vcommas ds

prettyList :: forall f a. Foldable f => Pretty a => f a -> Doc
prettyList xs = commas (pretty <$> fromFoldable xs)

class Highlightable a where
   highlightIf :: a -> Endo Doc

-- TODO: use Ann and Highlightable from Val
-- currently they are defined in reference to old Doc
class (Highlightable a, BoundedLattice a) <= Ann a

instance Ann Boolean
instance Ann Unit

instance Highlightable a => Highlightable (a × b) where
   highlightIf (a × _) doc = highlightIf a doc

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
   pretty (E.Let (E.VarDef o e) e') = text "def" <+> pretty o <> block (pretty e) <+++> pretty e'
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

instance Highlightable a => Pretty (Env a) where
   pretty (Env γ) = brackets $ go (toUnfoldable γ)
      where
      go :: List (Var × Val a) -> Doc
      go Nil = mempty
      go ((x × v) : rest) =
         (text x <+> text "->" <+> pretty v <+> text ",") <++> go rest

instance Highlightable a => Pretty (EnvExpr a) where
   pretty (EnvExpr γ e) = (pretty γ) <++> (pretty e)

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty (x ↦ σ) = pretty x <> pretty ":" <+> pretty σ

instance Highlightable a => Pretty (Val a) where
   pretty (Val a Nothing u) = highlightIf a (pretty u)
   pretty (Val a (Just v') u) = text "@doc" <> parens (pretty v') <+> highlightIf a (pretty u)

instance Highlightable a => Pretty (Var × (a × Val a)) where
   pretty (k × (a × v)) = highlightIf a (pretty k) <> text ":" <+> pretty v -- ???

instance Highlightable a => Pretty (BaseVal a) where
   pretty (V.Int n) = number n
   pretty (V.Float n) = number n
   pretty (V.Str str) = string str
   pretty (V.Dictionary (DictRep svs)) = record (pretty <$> (toUnfoldable svs))
   pretty (V.Constr c vs) = prettyConstr c vs
   pretty (V.Matrix (MatrixRep (vss × _ × _))) = vcommas $ fromFoldable (prettyList <$> vss) -- ???
   pretty (V.Fun phi) = pretty phi

instance Highlightable a => Pretty (Fun a) where
   pretty (V.Closure _ _ _) = text "cl"
   pretty (V.Foreign phi _) = pretty phi
   pretty (V.PartialConstr c vs) = prettyConstr c vs

instance Pretty ForeignOp where
   pretty (ForeignOp (s × _)) = pretty s

compare :: forall a. BotOf a a => Neg a => MeetSemilattice a => Eq a => Pretty a => String -> String -> a -> a -> String × String
compare op1 op2 x y =
   let
      x_minus_y × y_minus_x = symmetricDiff x y
      left = if x_minus_y == botOf x then "" else op1 <> " but not " <> op2 <> ":\n" <> prettyP x_minus_y
      right = if y_minus_x == botOf x then "" else op2 <> " but not " <> op1 <> ":\n" <> prettyP y_minus_x
   in
      left × right
