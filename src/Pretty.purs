module Pretty (class Pretty, pretty, prettyP, ExprType(..)) where

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

emptyDoc :: Doc
emptyDoc = empty 0 0

data InFront = Prefix String | Unit
type IsPair = Boolean × List Pattern
newtype FirstGroup a = First (RecDefs a)
type IsMatch a = Boolean × Clause a
data ExprType = Simple | Expression
type Sem = ExprType -> Doc

exprType :: forall a. Expr a -> ExprType
exprType (Var _) = Simple -- try 
exprType (Op _) = Simple -- need parentheses around Op otherwise some cases do not even parse 
exprType (Int _ _) = Simple -- try  
exprType (Float _ _) = Simple -- try 
exprType (Str _ _) = Simple
exprType (Constr _ _ Nil) = Simple -- try (if Simple then flatten test fails)
exprType (Constr _ _ _) = Expression
exprType (Record _ _) = Simple
exprType (Dictionary _ _) = Simple
exprType (Matrix _ _ _ _) = Simple
exprType (Lambda _) = Simple
exprType (Project _ _) = Expression
exprType (App _ _) = Expression
exprType (BinaryApp _ _ _) = Expression
exprType (MatchAs _ _) = Simple
exprType (IfElse _ _ _) = Simple
exprType (ListEmpty _) = Simple -- try
exprType (ListNonEmpty _ _ _) = Simple
exprType (ListEnum _ _) = Simple
exprType (ListComp _ _ _) = Simple
exprType (Let _ _) = Expression
exprType (LetRec _ _) = Expression

infixl 5 beside as .<>.
infixl 5 space as :--:
infixl 5 atop as .-.

class Pretty p where
   pretty :: p -> Sem

instance Ann a => Pretty (Expr a) where
   pretty s Simple = case exprType s of
      Simple -> pretty s Expression
      Expression -> parentheses (pretty s Expression)
   pretty (Var x) Expression = emptyDoc :--: text x :--: emptyDoc
   pretty (Op op) Expression = parentheses (text op)
   pretty (Int ann n) Expression = highlightIf ann $ text (show n)
   pretty (Float ann n) Expression = highlightIf ann $ text (show n)
   pretty (Str ann str) Expression = highlightIf ann $ slashes (text str)
   pretty (Constr ann c x) Expression = prettyConstr ann c x
   pretty (Record ann xss) Expression = highlightIf ann $ curlyBraces (pretty (false × xss) Expression) -- recursive call to pretty made 
   pretty (Dictionary ann sss) Expression = highlightIf ann $ dictBrackets (pretty sss Expression) -- recursive call to pretty made 
   pretty (Matrix ann e (x × y) e') Expression = highlightIf ann $ arrayBrackets (pretty e Expression .<>. text str.bar .<>. text str.lparenth .<>. text x .<>. text str.comma .<>. text y .<>. text str.rparenth :--: text str.in_ :--: pretty e' Expression)
   pretty (Lambda cs) Expression = parentheses (text str.fun :--: pretty cs Expression) -- recursive call to pretty made 
   pretty (Project s x) Expression = pretty s Expression .<>. text str.dot .<>. text x
   --pretty (App s s') Expression = pretty s Expression :--: parentheses (pretty s' Expression)
   pretty (App s s') Expression = pretty s Simple :--: pretty s' Simple
   pretty (BinaryApp s op s') Expression =  (pretty s Simple :--: checkOp op :--: pretty s' Simple)
   -- pretty (BinaryApp s op s') Expression = parentheses (pretty s Expression :--: checkOp op :--: pretty s' Expression)
   pretty (MatchAs s cs) Expression = ((text str.match :--: parentheses (pretty s Expression) :--: text str.as)) .-. curlyBraces (pretty cs Expression)
   pretty (IfElse s1 s2 s3) Expression = emptyDoc :--: text str.if_ :--: pretty s1 Expression :--: text str.then_ :--: pretty s2 Expression :--: text str.else_ :--: pretty s3 Expression
   pretty (ListEmpty ann) Expression = highlightIf ann $ brackets emptyDoc
   pretty (ListNonEmpty ann (Record _ xss) l) Expression = emptyDoc :--: (((highlightIf ann $ text str.lBracket) .<>. (highlightIf ann $ curlyBraces (pretty (true × xss) Expression))) .-. pretty l Expression)
   pretty (ListNonEmpty ann e l) Expression = emptyDoc :--: (highlightIf ann $ text str.lBracket) .<>. pretty e Expression .<>. pretty l Expression
   pretty (ListEnum s s') Expression = brackets (pretty s Expression .<>. text str.ellipsis .<>. pretty s' Expression)
   pretty (ListComp ann s qs) Expression = highlightIf ann $ brackets (pretty s Expression .<>. text str.bar .<>. pretty qs Expression)
   pretty (Let ds s) Expression = text str.let_ :--: pretty ds Expression :--: text str.in_ :--: pretty s Expression
   pretty (LetRec h s) Expression = (text str.let_ :--: pretty (First h) Expression) .-. text str.in_ :--: pretty s Expression

instance Ann a => Pretty (Boolean × List (Bind (Expr a))) where
   pretty (_ × (Cons s Nil)) _ = text (key s) .<>. text str.colon .<>. pretty (val s) Expression
   pretty (false × (Cons s xss)) _ = (text (key s) .<>. text str.colon .<>. pretty (val s) Expression .<>. text str.comma) .-. pretty (false × xss) Expression
   pretty (true × (Cons s xss)) _ = text (key s) .<>. text str.colon .<>. pretty (val s) Expression .<>. text str.comma .<>. pretty (true × xss) Expression
   pretty (_ × Nil) _ = emptyDoc

instance Ann a => Pretty (ListRest a) where
   pretty (Next ann (Record _ xss) l) _ = (highlightIf ann $ text str.comma) .<>. (highlightIf ann $ curlyBraces (pretty (true × xss) Expression)) .-. pretty l Expression
   pretty (Next ann s l) _ = (highlightIf ann $ text str.comma) .<>. pretty s Expression .<>. pretty l Expression
   pretty (End ann) _ = highlightIf ann $ text str.rBracket

instance Ann a => Pretty (List (Pair (Expr a))) where
   pretty (Cons (Pair e e') Nil) _ = pretty e Expression :--: text str.colonEq :--: pretty e' Expression
   pretty (Cons (Pair e e') sss) _ = pretty e Expression :--: text str.colonEq :--: pretty e' Expression .<>. text str.comma :--: pretty sss Expression
   pretty Nil _ = emptyDoc

instance Pretty Pattern where
   pretty (PVar x) _ = text x
   pretty (PRecord xps) _ = curlyBraces (pretty xps Expression)
   pretty (PConstr c ps) _ = case c == cPair of
      true -> parentheses (pretty (true × ps) Expression)
      false -> case c == "Empty" of
         true -> text c .<>. pretty (false × ps) Expression
         false -> case c == str.colon of
            true -> parentheses (pretty ps Expression)
            false -> parentheses (text c :--: pretty (false × ps) Expression)
   pretty (PListEmpty) _ = brackets emptyDoc
   pretty (PListNonEmpty p l) _ = brackets (pretty p Expression .<>. pretty l Expression)

instance Pretty (List (Bind (Pattern))) where
   pretty (Cons xp Nil) _ = text (key xp) .<>. text str.colon .<>. pretty (val xp) Expression
   pretty (Cons x xps) _ = text (key x) .<>. text str.colon .<>. pretty (val x) Expression .<>. text str.comma .-. pretty xps Expression
   pretty Nil _ = emptyDoc

instance Pretty IsPair where
   pretty (_ × Nil) _ = emptyDoc
   pretty (_ × (Cons p Nil)) _ = pretty p Expression
   pretty (true × (Cons p ps)) _ = pretty p Expression .<>. text str.comma .<>. pretty (true × ps) Expression
   pretty (false × (Cons p ps)) _ = pretty p Expression :--: pretty (false × ps) Expression

instance Pretty (List Pattern) where
   pretty (Cons p Nil) _ = pretty p Expression
   pretty (Cons p ps) _ = pretty p Expression .<>. text str.colon .<>. pretty ps Expression
   pretty Nil _ = emptyDoc

instance Pretty ListRestPattern where
   pretty (PNext p l) _ = text str.comma .<>. pretty p Expression .<>. pretty l Expression
   pretty PEnd _ = emptyDoc

instance Ann a => Pretty (Boolean × Clause a) where
   pretty (true × Clause (ps × e)) _ = pretty (false × toList (ps)) Expression :--: text str.rArrow :--: pretty e Expression
   pretty (false × Clause (ps × e)) _ = pretty (false × (toList ps)) Expression :--: text str.equal :--: pretty e Expression

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) _ = intersperse' (toList (map (flip pretty Expression) (map (\x -> false × x) cs))) (text str.semiColon)

instance Ann a => Pretty (Branch a) where
   pretty (x × Clause (ps × e)) _ = text x :--: pretty (false × Clause (ps × e)) Expression

instance Ann a => Pretty (NonEmptyList (Branch a)) where
   pretty h _ = intersperse' (toList (map (flip pretty Expression) h)) (text str.semiColon)

instance Ann a => Pretty (NonEmptyList (NonEmptyList (Branch a))) where
   pretty hs _ = intersperse' (toList (map (flip pretty Expression) hs)) (text str.semiColon)

instance Ann a => Pretty (FirstGroup a) where
   pretty (First h) _ = pretty (groupBy (\p q -> key p == key q) h) Expression

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty pss _ = intersperse' (map (flip pretty Expression) (map (\x -> true × x) (map Clause (toList (helperMatch pss))))) (text str.semiColon)

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef p s) _ = pretty p Expression :--: text str.equal :--: pretty s Expression

instance Ann a => Pretty (VarDefs a) where
   pretty ds _ = intersperse' (toList (map (flip pretty Expression) ds)) (text str.semiColon)

instance Ann a => Pretty (List (Expr a)) where
   pretty (Cons s Nil) _ = pretty s Expression
   pretty (Cons s ss) _ = pretty s Expression :--: pretty ss Expression
   pretty Nil _ = emptyDoc

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (Guard s) Nil) _ = pretty s Expression
   pretty (Cons (Declaration d) Nil) _ = text str.let_ :--: pretty d Expression
   pretty (Cons (Generator p s) Nil) _ = pretty p Expression :--: text str.lArrow :--: pretty s Expression
   pretty (Cons (Guard s) qs) _ = pretty s Expression .<>. text str.comma :--: pretty qs Expression
   pretty (Cons (Declaration d) qs) _ = text str.let_ :--: pretty d Expression .<>. text str.comma :--: pretty qs Expression
   pretty (Cons (Generator p s) qs) _ = pretty p Expression :--: text str.lArrow :--: pretty s Expression .<>. text str.comma :--: pretty qs Expression
   pretty Nil _ = emptyDoc

checkOp :: String -> Doc
checkOp x = case (member x (keys (opDefs))) of
   true -> text x
   false -> backTicks (text x)

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons dc Nil) _ = dc
intersperse' (Cons dc dcs) dc' = dc .<>. dc' .-. intersperse' dcs dc'
intersperse' Nil _ = emptyDoc

helperMatch :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
helperMatch pss = map (\(x × y) -> singleton x × y) pss

-- ======================================
-- Legacy Implementation : to be replaced 
-- ======================================

prettyP :: forall d. Pretty d => d -> String
prettyP x = render (pretty x Expression)

between :: Doc -> Doc -> Endo Doc
between l r doc = l .<>. doc .<>. r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

dictBrackets :: Endo Doc
dictBrackets = between (text str.dictLBracket) (text str.dictRBracket)

parentheses :: Endo Doc
parentheses = between (text str.lparenth) (text str.rparenth)

slashes :: Endo Doc
slashes = between (text str.slash) (text str.slash)

backTicks :: Endo Doc
backTicks = between (text str.backtick) (text str.backtick)

curlyBraces :: Endo Doc
curlyBraces = between (text str.curlylBrace) (text str.curlyrBrace)

arrayBrackets :: Endo Doc
arrayBrackets = between (text str.arrayLBracket) (text str.arrayRBracket)

comma :: Doc
comma = text str.comma

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
   pretty x _ = text x

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
   doc = pretty x Expression

nil :: Doc
nil = text (str.lBracket <> str.rBracket)

prettyConstr :: forall d a. Pretty d => Highlightable a => a -> Ctr -> List d -> Doc
prettyConstr α c (x : y : ys)
   | c == cPair = assert (null ys) $ highlightIf α $ parens (hcomma [ pretty x Expression, pretty y Expression ])
prettyConstr α c ys
   | c == cNil = assert (null ys) $ highlightIf α nil
prettyConstr α c (x : y : ys)
   | c == cCons = assert (null ys) $ parens (hspace [ pretty x Expression, highlightIf α $ text ":", pretty y Expression ])
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
   xvs <#> first prettyKey <#> (\(x × v) -> hspace [ x .<>. sep, pretty v Expression ])
      # hcomma >>> bracify >>> highlightIf α

prettyDict :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyDict = between (text str.dictLBracket) (text str.dictRBracket) # prettyRecordOrDict (text str.colonEq)

prettyRecord :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyRecord = between (text "{") (text "}") # prettyRecordOrDict (text str.colon)

instance Highlightable a => Pretty (E.Expr a) where
   pretty (E.Var x) _ = text x
   pretty (E.Int α n) _ = highlightIf α (text (show n))
   pretty (E.Float _ n) _ = text (show n)
   pretty (E.Str _ str) _ = text (show str)
   pretty (E.Record α xes) _ = prettyRecord text α (xes # D.toUnfoldable)
   -- prettyDict pretty α (ees <#> toTuple) 
   pretty (E.Dictionary α ees) _ = prettyDict (flip pretty Expression) α (ees <#> toTuple)
   pretty (E.Constr α c es) _ = prettyConstr α c es
   pretty (E.Matrix _ _ _ _) _ = error "todo"
   pretty (E.Lambda σ) _ = hspace [ text str.fun, pretty σ Expression ]
   pretty (E.Op op) _ = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') _ = atop (hspace [ text str.let_, pretty σ Expression, text str.equals, pretty e Expression, text str.in_ ])
      (pretty e' Expression)
   pretty (E.LetRec δ e) _ = atop (hspace [ text str.let_, pretty δ Expression, text str.in_ ]) (pretty e Expression)
   pretty (E.Project e x) _ = pretty e Expression .<>. text str.dot .<>. pretty x Expression
   pretty (E.App e e') _ = hspace [ pretty e Expression, pretty e' Expression ]
   pretty (E.Sugar _ e) _ = pretty e Expression

instance Highlightable a => Pretty (Dict (Elim a)) where
   pretty x _ = go (D.toUnfoldable x)
      where
      go :: List (Var × Elim a) -> Doc
      go Nil = error absurd -- non-empty
      go (xσ : Nil) = pretty xσ Expression
      go (xσ : δ) = atop (go δ .<>. semi) (pretty xσ Expression)

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty (x ↦ σ) _ = hspace [ text x, text str.equals, pretty σ Expression ]

instance Highlightable a => Pretty (Cont a) where
   pretty ContNone _ = emptyDoc
   pretty (ContExpr e) _ = pretty e Expression
   pretty (ContElim σ) _ = pretty σ Expression

instance Highlightable a => Pretty (Ctr × Cont a) where
   pretty (c × κ) _ = hspace [ text (showCtr c), text str.rArrow, pretty κ Expression ]

instance Highlightable a => Pretty (Elim a) where
   pretty (ElimVar x κ) _ = hspace [ text x, text str.rArrow, pretty κ Expression ]
   pretty (ElimConstr κs) _ = hcomma (flip pretty Expression <$> κs) -- looks dodgy
   pretty (ElimSug _ κ) _ = pretty κ Expression
   pretty (ElimRecord _ _) _ = error "todo"

instance Highlightable a => Pretty (Val a) where
   pretty (V.Int α n) _ = highlightIf α (text (show n))
   pretty (V.Float α n) _ = highlightIf α (text (show n))
   pretty (V.Str α str) _ = highlightIf α (text (show str))
   pretty (V.Record α xvs) _ = prettyRecord text α (xvs # D.toUnfoldable)
   pretty (V.Dictionary α svs) _ = prettyDict
      (\(s × β) -> highlightIf β (text (show s)))
      α
      (svs # D.toUnfoldable <#> \(s × (β × v)) -> (s × β) × v)
   pretty (V.Constr α c vs) _ = prettyConstr α c vs
   pretty (V.Matrix _ (vss × _ × _)) _ = vert comma (((<$>) (flip pretty Expression) >>> hcomma) <$> vss)
   pretty (V.Fun φ) _ = pretty φ Expression

instance Highlightable a => Pretty (Fun a) where
   pretty (V.Closure _ _ _ _) _ = text "<closure>"
   pretty (V.Foreign φ _) _ = parens (runExists pretty φ Expression)
   pretty (V.PartialConstr α c vs) _ = prettyConstr α c vs

instance Pretty (ForeignOp' t) where
   pretty _ _ = text "<extern op>" -- TODO

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
--    pretty (S.Lambda cs) = hspace [ text str.fun, vert semi (pretty <$> unwrap cs) ]
--    pretty (S.Project s x) = pretty s :<>: text (str.dot <> x)
--    pretty (S.App s s') = hspace [ pretty s, pretty s' ]
--    pretty (S.BinaryApp s op s') = parens (hspace [ pretty s, text op, pretty s' ])
--    pretty (S.MatchAs s cs) = atop (hspace [ text str.match, pretty s, text str.as ]) (vert semi (pretty <$> cs))
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
--    toList _ = error acsurd

-- instance ToPair S.Pattern where
--    toPair (S.PConstr c (p : p' : Nil)) | c == cPair = p × p'
--    toPair _ = error acsurd

-- instance Pretty S.ListRestPattern where
--    pretty S.PEnd = text str.rBracket
--    pretty (S.PNext s l) = hspace [ comma, pretty s :<>: pretty l ]

