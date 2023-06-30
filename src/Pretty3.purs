module Pretty3 (class Pretty, pretty, prettyP) where

import Prelude hiding (absurd, between)

import Bindings (Bind, key, val, Var, (↦))
import Data.Exists (runExists)
import Data.Foldable (class Foldable)
import Data.List (List(..), fromFoldable, (:), null)
import Data.List.NonEmpty (NonEmptyList, groupBy, singleton, toList)
import Data.Map (keys)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong (first)
import Data.Set (member)
import Data.String (Pattern(..), contains) as Data.String
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Dict (Dict)
import Dict (toUnfoldable) as D
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import Parse (str)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), VarDef(..), VarDefs, Qualifier(..), RecDefs)
import Util (type (+), type (×), Endo, absurd, assert, error, intersperse, (×))
import Util.Pair (Pair(..), toTuple)
import Util.Pretty (Doc, atop, beside, empty, space, text, render, hcat)
import Val (Fun(..), Val(..)) as V
import Val (class Ann, class Highlightable, ForeignOp', Fun, Val, highlightIf)

-- import Data.List.NonEmpty (toList) as NEL
-- import Data.Newtype (unwrap)

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

instance Ann a => Pretty (Expr a) where
   pretty (Int ann n) = highlightIf ann $ text (show n)
   pretty (App s s') = (pretty s :--:text "(") .<>. pretty s' .<>. text ")"
   pretty (Var x) = emptyDoc :--: text x :--: emptyDoc
   pretty (Op x) = text "(" .<>. text x .<>. text ")"
   pretty (BinaryApp s x s') = text "(" .<>. (pretty s :--: checkOp x :--: pretty s') .<>. text ")" -- edited
   pretty (IfElse s s_1 s_2) = emptyDoc :--: text "if" :--: pretty s :--: text "then" :--:  pretty s_1 :--: text "else" :--: pretty s_2
   pretty (Project s x) = pretty s .<>. text "." .<>. text x
   pretty (Record ann x) = highlightIf ann $ text "{" .<>. pretty (false × x) .<>. text "}"
   pretty (Lambda (Clauses cs)) = text "(" .<>. (text "fun" :--: pretty (Clauses cs)) .<>. text ")" :--: emptyDoc -- edited
   pretty (LetRec g s) = (text "let" :--: pretty (First g)) .-. text "in" :--: pretty s
   pretty (MatchAs s x) = ((text "match" :--: text "(" .<>. pretty s .<>. text ")"  :--: text "as {")) .-.  (pretty x) .-. text "}"
   pretty (ListEmpty ann) = highlightIf ann $ text "[]"
   pretty (ListNonEmpty ann (Record _ x) y) = emptyDoc :--: (((highlightIf ann $ text "[") .<>. (highlightIf ann $ text "{" .<>. pretty (true × x) .<>. text "}")) .-. pretty y)
   pretty (ListNonEmpty ann s x) = emptyDoc :--: (highlightIf ann $ text "[") .<>. pretty s .<>. pretty x
   pretty (ListEnum s s') = text "[" .<>. pretty s .<>. text ".." .<>. pretty s' .<>. text "]"
   pretty (Let x s) = text "(" .<>. text "let" :--: pretty x  :--: text "in" :--: pretty s .<>. text ")"
   pretty (Matrix ann s (v × v') s') = highlightIf ann $ text "[" .<>. text "|" .<>. pretty s .<>. text "|" .<>. text "(" .<>. text v .<>. text "," .<>. text v' .<>. (text ")" :--: text "in" :--: pretty s') .<>. text "|" .<>. text "]"
   pretty (Constr ann c x) = prettyConstr ann c x
   pretty (Dictionary ann x) = highlightIf ann $ text "{" .<>. (text "|" :--: pretty x :--: text "|") .<>. text "}"
   pretty (Str ann x) = highlightIf ann $ text "\"" .<>. text x .<>. text "\""
   pretty (Float ann x) = highlightIf ann $ text (show x)
   pretty (ListComp ann s q) = highlightIf ann $ text "[" .<>. pretty s .<>. text "|" .<>. pretty q .<>. text "]"

instance Ann a => Pretty (Boolean × List (Bind (Expr a))) where
   pretty (_ × (Cons x Nil)) = text (key x) .<>. text ":" .<>. pretty (val x)
   pretty (false × (Cons x xs)) = (text (key x) .<>. text ":" .<>. pretty (val x) .<>. text ",") .-. pretty (false × xs) -- edited atop
   pretty (true × (Cons x xs)) = (text (key x) .<>. text ":" .<>. pretty (val x) .<>. text ",") .<>. pretty (true × xs)
   pretty (_ × Nil) = emptyDoc

instance Ann a => Pretty (ListRest a) where
   pretty (Next ann (Record _ x) y) = (highlightIf ann $ text ",") .<>. text "" .<>. (highlightIf ann $ text "{" .<>. pretty (true × x) .<>. text "}") .-. pretty y
   pretty (Next ann s x) = (highlightIf ann $ text ",") .<>. text "" .<>. pretty s .<>. pretty x
   pretty (End ann) = highlightIf ann $ text "]"

instance Ann a => Pretty (List (Pair (Expr a))) where
   pretty (Cons (Pair e e') Nil) = pretty e  :--: text ":=" :--: pretty e'
   pretty (Cons (Pair e e') xs) = pretty e :--: text ":=" :--: pretty e' .<>. (text "," :--: pretty xs)
   pretty Nil = emptyDoc

instance Pretty (Pattern) where
   pretty (PVar x) = text x
   pretty (PRecord x) = text "{" .<>. pretty x .<>. text "}"
   pretty (PConstr "Pair" x) = text "(" .<>. pretty (true × x) .<>. text ")"
   pretty (PConstr "Empty" x) = text "Empty" .<>. pretty (false × x)
   pretty (PConstr ":" x) = text "(" .<>. pretty x .<>. text ")"
   pretty (PConstr c x) = text "(" .<>. (text c :--: pretty (false × x)) .<>. text ")"
   pretty (PListEmpty) = text "[]"
   pretty (PListNonEmpty p x) = text "[" .<>. pretty p .<>. pretty x .<>. text "]"

instance Pretty (List (Bind (Pattern))) where
   pretty (Cons x Nil) = text (key x) .<>. text ":" .<>. pretty (val x) 
   pretty (Cons x xs) = text (key x) .<>. text ":" .<>. pretty (val x) .<>. text "," .-. pretty xs
   pretty Nil = emptyDoc

instance Pretty (IsPair) where
   pretty (_ × Nil) = emptyDoc
   pretty (_ × (Cons x Nil)) = pretty x
   pretty (true × (Cons x xs)) = (pretty x .<>. text ",") .<>. pretty (true × xs)
   pretty (false × (Cons x xs)) = pretty x :--: pretty (false × xs)

instance Pretty (List Pattern) where
   pretty (Cons x Nil) = pretty x
   pretty (Cons x xs) = pretty x .<>. text ":" .<>. pretty xs
   pretty Nil = emptyDoc

instance Pretty (ListRestPattern) where
   pretty (PNext p x) = text "," .<>. pretty p .<>. pretty x
   pretty PEnd = emptyDoc

instance Ann a => Pretty (Boolean × Clause a) where
   pretty (true × Clause (ps × e)) = pretty (false × toList (ps)) :--: text "->" :--: pretty e
   pretty (false × Clause (ps × e)) = pretty (false × (toList ps)) :--: text "=" :--: pretty e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) = intersperse' (toList (map pretty (map (\x -> false × x) cs))) (text ";")

instance Ann a => Pretty (Branch a) where
   pretty (x × Clause (ps × e)) = text x :--: pretty (false × Clause (ps × e))

instance Ann a => Pretty (NonEmptyList (Branch a)) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Ann a => Pretty (NonEmptyList (NonEmptyList (Branch a))) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Ann a => Pretty (FirstGroup a) where
   pretty (First x) = pretty (groupBy (\p q -> key p == key q) x)

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty x = intersperse' (map pretty (map helperMatch2 (map Clause (toList (helperMatch x))))) (text ";")

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef p s) = pretty p :--: text "=":--: pretty s

instance Ann a => Pretty (VarDefs a) where
   pretty x = intersperse' (toList (map pretty x)) (text ";")

instance Ann a => Pretty (IsConstrPair a) where
   pretty (_ × ((Cons x Nil))) = pretty x
   pretty (true × (Cons x xs)) = pretty x  :--: pretty (true × xs)
   pretty (false × (Cons x xs)) = pretty x .<>. text ":" .<>. pretty (false × xs)
   pretty (_ × Nil) = emptyDoc

instance Ann a => Pretty (List (Expr a)) where
   pretty (Cons x Nil) = pretty x
   pretty (Cons x xs) = pretty x :--: pretty xs
   pretty Nil = emptyDoc

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (Guard s) Nil) = pretty s
   pretty (Cons (Declaration v) Nil) = text "let" :--: pretty v
   pretty (Cons (Generator p s) Nil) = pretty p .<>. text "<-" .<>. pretty s
   pretty (Cons (Guard s) xs) = pretty s .<>. text "," .<>. pretty xs
   pretty (Cons (Declaration v) xs) = text "let" :--: pretty v .<>. text "," .<>. pretty xs
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

-- ======================================
-- Legacy Implementation : to be replaced 
-- ======================================

prettyP :: forall d. Pretty d => d -> String
prettyP = pretty >>> render

between :: Doc -> Doc -> Endo Doc
between l r doc = l .<>. doc .<>. r

-- brackets :: Endo Doc
-- brackets = between (text str.lBracket) (text str.rBracket)

comma :: Doc
comma = text ","

semi :: Doc
semi = text ";"

space2 :: Doc
space2 = text " "

hspace :: forall f. Foldable f => f Doc -> Doc
hspace = fromFoldable >>> intersperse space2 >>> hcat

hcomma :: forall f. Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse (comma .<>. space2) >>> hcat

parens :: Endo Doc
parens = between (text "(") (text ")")

class ToList a where
   toList2 :: a -> List a

class ToPair a where
   toPair :: a -> a × a

instance ToPair (E.Expr a) where
   toPair (E.Constr _ c (e : e' : Nil)) | c == cPair = e × e'
   toPair _ = error absurd

instance ToPair (Val a) where
   toPair (V.Constr _ c (v : v' : Nil)) | c == cPair = v × v'
   toPair _ = error absurd

instance Pretty String where
   pretty = text

vert :: forall f. Foldable f => Doc -> f Doc -> Doc
vert delim = fromFoldable >>> vert'
   where
   vert' :: List Doc -> Doc
   vert' Nil = emptyDoc
   vert' (x : Nil) = x
   vert' (x : y : xs) = atop (x .<>. delim) (vert' (y : xs))

prettyCtr :: Ctr -> Doc
prettyCtr = showCtr >>> text

-- Cheap hack; revisit.
prettyParensOpt :: forall a. Pretty a => a -> Doc
prettyParensOpt x =
   if Data.String.contains (Data.String.Pattern " ") (render doc) then parens doc
   else doc
   where
   doc = pretty x

nil :: Doc
nil = text (str.lBracket <> str.rBracket)

prettyConstr :: forall d a. Pretty d => Highlightable a => a -> Ctr -> List d -> Doc
prettyConstr α c (x : y : ys)
   | c == cPair = assert (null ys) $ highlightIf α $ parens (hcomma [ pretty x, pretty y ])
prettyConstr α c ys
   | c == cNil = assert (null ys) $ highlightIf α nil
prettyConstr α c (x : y : ys)
   | c == cCons = assert (null ys) $ parens (hspace [ pretty x, highlightIf α $ text ":", pretty y ])
prettyConstr α c xs = hspace (highlightIf α (prettyCtr c) : (prettyParensOpt <$> xs))

prettyRecordOrDict
   :: forall d b a
    . Pretty d
   => Highlightable a
   => Doc
   -> Endo Doc
   -> (b -> Doc)
   -> a
   -> List (b × d)
   -> Doc
prettyRecordOrDict sep bracify prettyKey α xvs =
   xvs <#> first prettyKey <#> (\(x × v) -> hspace [ x .<>. sep, pretty v ])
      # hcomma >>> bracify >>> highlightIf α

prettyDict :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyDict = between (text str.dictLBracket) (text str.dictRBracket) # prettyRecordOrDict (text str.colonEq)

prettyRecord :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyRecord = between (text "{") (text "}") # prettyRecordOrDict (text str.colon)

instance Highlightable a => Pretty (E.Expr a) where
   pretty (E.Var x) = text x
   pretty (E.Int α n) = highlightIf α (text (show n))
   pretty (E.Float _ n) = text (show n)
   pretty (E.Str _ str) = text (show str)
   pretty (E.Record α xes) = prettyRecord text α (xes # D.toUnfoldable)
   pretty (E.Dictionary α ees) = prettyDict pretty α (ees <#> toTuple)
   pretty (E.Constr α c es) = prettyConstr α c es
   pretty (E.Matrix _ _ _ _) = error "todo"
   pretty (E.Lambda σ) = hspace [ text str.fun, pretty σ ]
   pretty (E.Op op) = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hspace [ text str.let_, pretty σ, text str.equals, pretty e, text str.in_ ])
      (pretty e')
   pretty (E.LetRec δ e) = atop (hspace [ text str.let_, pretty δ, text str.in_ ]) (pretty e)
   pretty (E.Project e x) = pretty e .<>. text str.dot .<>. pretty x
   pretty (E.App e e') = hspace [ pretty e, pretty e' ]
   pretty (E.Sugar _ e) = pretty e

instance Highlightable a => Pretty (Dict (Elim a)) where
   pretty = D.toUnfoldable >>> go
      where
      go :: List (Var × Elim a) -> Doc
      go Nil = error absurd -- non-empty
      go (xσ : Nil) = pretty xσ
      go (xσ : δ) = atop (go δ .<>. semi) (pretty xσ)

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty (x ↦ σ) = hspace [ text x, text str.equals, pretty σ ]

instance Highlightable a => Pretty (Cont a) where
   pretty ContNone = emptyDoc
   pretty (ContExpr e) = pretty e
   pretty (ContElim σ) = pretty σ

instance Highlightable a => Pretty (Ctr × Cont a) where
   pretty (c × κ) = hspace [ text (showCtr c), text str.rArrow, pretty κ ]

instance Highlightable a => Pretty (Elim a) where
   pretty (ElimVar x κ) = hspace [ text x, text str.rArrow, pretty κ ]
   pretty (ElimConstr κs) = hcomma (pretty <$> κs) -- looks dodgy
   pretty (ElimSug _ κ) = pretty κ
   pretty (ElimRecord _ _) = error "todo"

instance Highlightable a => Pretty (Val a) where
   pretty (V.Int α n) = highlightIf α (text (show n))
   pretty (V.Float α n) = highlightIf α (text (show n))
   pretty (V.Str α str) = highlightIf α (text (show str))
   pretty (V.Record α xvs) = prettyRecord text α (xvs # D.toUnfoldable)
   pretty (V.Dictionary α svs) = prettyDict
      (\(s × β) -> highlightIf β (text (show s)))
      α
      (svs # D.toUnfoldable <#> \(s × (β × v)) -> (s × β) × v)
   pretty (V.Constr α c vs) = prettyConstr α c vs
   pretty (V.Matrix _ (vss × _ × _)) = vert comma (((<$>) pretty >>> hcomma) <$> vss)
   pretty (V.Fun φ) = pretty φ

instance Highlightable a => Pretty (Fun a) where
   pretty (V.Closure _ _ _ _) = text "<closure>"
   pretty (V.Foreign φ _) = parens (runExists pretty φ)
   pretty (V.PartialConstr α c vs) = prettyConstr α c vs

instance Pretty (ForeignOp' t) where
   pretty _ = text "<extern op>" -- TODO

instance (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty

-- -- Surface language

-- instance Highlightable a => ToPair (S.Expr a) where
--    toPair (S.Constr _ c (s : s' : Nil)) | c == cPair = s × s'
--    toPair s = error ("Not a pair: " <> prettyP s)

-- instance Highlightable a => Pretty (S.Expr a) where
--    pretty (S.Var x) = text x
--    pretty (S.Op op) = parens (text op)
--    pretty (S.Int α n) = highlightIf α (text (show n))
--    pretty (S.Float α n) = highlightIf α (text (show n))
--    pretty (S.Str α str) = highlightIf α (text (show str))
--    pretty (S.Constr α c ss) = prettyConstr α c ss
--    pretty (S.Record α xss) = prettyRecord text α xss
--    pretty (S.Dictionary α sss) = prettyDict pretty α (sss <#> toTuple)
--    pretty (S.Matrix α e (x × y) e') = highlightIf α (hspace (init <> quant))
--       where
--       init = [ text str.arrayLBracket, pretty e, text str.bar ]
--       quant = [ parens (hcomma [ text x, text y ]), text (str.in_), pretty e', text str.arrayRBracket ]
--    pretty (S.Lambda bs) = hspace [ text str.fun, vert semi (pretty <$> unwrap bs) ]
--    pretty (S.Project s x) = pretty s :<>: text (str.dot <> x)
--    pretty (S.App s s') = hspace [ pretty s, pretty s' ]
--    pretty (S.BinaryApp s op s') = parens (hspace [ pretty s, text op, pretty s' ])
--    pretty (S.MatchAs s bs) = atop (hspace [ text str.match, pretty s, text str.as ]) (vert semi (pretty <$> bs))
--    pretty (S.IfElse s1 s2 s3) =
--       hspace [ text str.if_, pretty s1, text str.then_, pretty s2, text str.else_, pretty s3 ]
--    pretty (S.ListEmpty α) = highlightIf α nil
--    pretty (S.ListNonEmpty α e l) = highlightIf α (text str.lBracket) :<>: pretty e :<>: pretty l
--    pretty (S.ListEnum s s') = brackets (hspace [ pretty s, text str.ellipsis, pretty s' ])
--    pretty (S.ListComp α s qs) = highlightIf α $ brackets (hspace [ pretty s, text str.bar, hcomma (pretty <$> qs) ])
--    pretty (S.Let ds s) = atop (hspace [ text str.let_, vert semi (pretty <$> ds) ])
--       (hspace [ text str.in_, pretty s ])
--    pretty (S.LetRec h s) = atop (hspace [ text str.let_, vert semi (pretty <$> h) ])
--       (hspace [ text str.in_, pretty s ])

-- instance Highlightable a => Pretty (S.ListRest a) where
--    pretty (S.End α) = highlightIf α (text str.rBracket)
--    pretty (S.Next α s l) = hspace [ highlightIf α comma, pretty s :<>: pretty l ]

-- instance Highlightable a => Pretty (String × (NonEmptyList S.Pattern × S.Expr a)) where
--    pretty (x × b) = hspace [ text x, pretty b ]

-- instance Highlightable a => Pretty (String × (S.Clause a)) where
--    pretty (x × b) = hspace [ text x, pretty b ]

-- instance Highlightable a => Pretty (S.Clause a) where
--    pretty (S.Clause (s × b)) = pretty (s × b)

-- instance Highlightable a => Pretty (NonEmptyList S.Pattern × S.Expr a) where
--    pretty (ps × s) = hspace ((pretty <$> NEL.toList ps) <> (text str.equals : pretty s : Nil))

-- instance Highlightable a => Pretty (S.VarDef a) where
--    pretty (S.VarDef p s) = hspace [ pretty p, text str.equals, pretty s ]

-- instance Highlightable a => Pretty (S.Pattern × S.Expr a) where
--    pretty (p × s) = pretty p :<>: text str.lArrow :<>: pretty s

-- instance Highlightable a => Pretty (S.Qualifier a) where
--    pretty (S.Guard e) = pretty e
--    pretty (S.Generator p e) = hspace [ pretty p, text str.lArrow, pretty e ]
--    pretty (S.Declaration (S.VarDef p e)) = hspace [ text str.let_, pretty p, text str.equals, pretty e ]

-- instance (Pretty a, Pretty b) => Pretty (a + b) where
--    pretty = pretty ||| pretty

-- instance Pretty S.Pattern where
--    pretty (S.PVar x) = text x
--    pretty (S.PConstr c ps) = prettyConstr false c ps
--    pretty (S.PRecord xps) = prettyRecord text false xps
--    pretty (S.PListEmpty) = nil
--    pretty (S.PListNonEmpty s l) = text str.lBracket :<>: pretty s :<>: pretty l

-- instance ToList S.Pattern where
--    toList (S.PConstr c (p : p' : Nil)) | c == cCons = p : toList p'
--    toList (S.PConstr c Nil) | c == cNil = Nil
--    toList _ = error absurd

-- instance ToPair S.Pattern where
--    toPair (S.PConstr c (p : p' : Nil)) | c == cPair = p × p'
--    toPair _ = error absurd

-- instance Pretty S.ListRestPattern where
--    pretty S.PEnd = text str.rBracket
--    pretty (S.PNext s l) = hspace [ comma, pretty s :<>: pretty l ]

