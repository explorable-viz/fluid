module Temp.Pretty (prettyPy) where

import Prelude

import Data.List (List(..), null, singleton, uncons, (:))
import Data.List.NonEmpty (NonEmptyList, head, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Temp.Pretty.Constants (_asterisk, _case, _colon, _comma, _def, _ellipsis, _else, _empty, _for, _if, _in, _match)
import Temp.Pretty.Doc (Doc(..), line, text, (<++>), (<+>))
import Temp.Pretty.Helpers (block, braces, brackets, hsep, hsepWith, num, parens, quotes', render, todo, vsep)
import Util (type (×), assert, (×))
import Val (class Ann)

class Pretty p where
   pretty :: p -> Doc

prettyPy :: forall a. Ann a => Expr a -> String
prettyPy x = render (pretty x)

binaryApp :: forall a. Ann a => Int -> Expr a -> Doc
binaryApp n (BinaryApp s op s') =
   case getPrec op of
      -1 -> binaryApp 0 s <+> text "|" <> text op <> text "|" <+> binaryApp 0 s'
      n' -> if n' <= n then parens (binaryApp n' s <+> text op <+> binaryApp n' s') else binaryApp n' s <+> text op <+> binaryApp n' s'
binaryApp _ e = pretty e

lambda :: forall a. Ann a => List Pattern -> Expr a -> Doc
lambda ps e = _def <+> (hsepWith _comma (pretty <$> ps)) <> _colon <+> pretty e

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = text x
   pretty (Op o) = text o
   pretty (Int _ _ n) = num n
   pretty (Float _ _ n) = num n
   pretty (Str _ _ str) = quotes' str
   pretty (Constr _ _ c Nil) = text c
   pretty (Constr _ _ c as) = prettyConstr c as
   pretty (Dictionary _ _ es) = braces (pretty es)
   pretty (Matrix _ _ _ _ _) = todo "Matrix"
   pretty (Lambda cs) = parens (pretty cs)
   pretty (Project _ s x) = pretty s <> brackets (quotes' x)
   pretty (DProject _ e k) = pretty e <> brackets (pretty k)
   pretty (App _ (Op op) s') = parens (lambda (PVar "x" : Nil) (BinaryApp s' op (Var "x")))
   pretty (App d s s') = prettyAppChain (App d s s') Nil
   pretty (BinaryApp s op s') = binaryApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = _match <+> pretty s <> block (pretty cs)
   pretty (IfElse i t e) = _if <+> pretty i <> block (pretty t) <++> _else <> block (pretty e)
   pretty (ListEmpty _ _) = _empty
   pretty (ListNonEmpty _ _ e l) = brackets (pretty e <> pretty l)
   pretty (ListEnum s s') = brackets (pretty s <+> _ellipsis <+> pretty s')
   pretty (ListComp _ _ s qs) = brackets (pretty s <+> pretty qs)
   pretty (Let ds s) = pretty ds <++> pretty s
   pretty (LetRec h s) = pretty h <++> pretty s

listCase :: List Pattern -> Doc
listCase Nil = Empty
listCase (Cons p Nil) = pretty p
listCase (Cons p (Cons p' Nil)) = pretty p <> _comma <+> _asterisk <> pretty p'
listCase (Cons p ps) = pretty p <> _comma <+> listCase ps

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (ListCompDecl (VarDef v s)) Nil) = _for <+> pretty v <+> _in <+> brackets (pretty s)
   pretty (Cons (ListCompGuard s) Nil) = _if <+> pretty s
   pretty (Cons (ListCompGen _ p s) Nil) = _for <+> pretty p <+> _in <+> pretty s
   pretty (Cons q qs) = pretty (singleton q) <+> pretty qs
   pretty Nil = mempty

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty pss = vsep (toList (defMatchCase' <$> pss))

getPrec :: String -> Int
getPrec x = case lookup x opDefs of
   Just y -> y.prec
   Nothing -> -1

instance Pretty Pattern where
   pretty (PVar x) = text x
   pretty (PRecord _) = todo "PRecord"
   pretty (PConstr c Nil) = text c
   pretty (PConstr "Pair" (x : y : Nil)) = parens (pretty x <> _comma <+> pretty y)
   pretty (PConstr c ps) = case uncons ps of
      Just { head: p, tail: Nil } -> text c <+> pretty p
      _ ->
         if c == cPair then parens $ prettyPattConstr (_comma) ps
         else if c == cCons then brackets (listCase ps)
         else parens $ text c <+> prettyPattConstr Empty ps
   pretty (PListEmpty) = _empty
   pretty (PListNonEmpty p l) = brackets (pretty p <> pretty l)

instance Ann a => Pretty (ListRest a) where
   pretty (Next _ (Dictionary _ _ _) _) = todo "listrestdict"
   pretty (Next _ s l) = _comma <+> pretty s <+> pretty l
   pretty (End _) = mempty

instance Pretty ListRestPattern where
   pretty (PListVar x) = text x
   pretty (PListNext p l) = _comma <+> pretty p <+> pretty l
   pretty PListEnd = mempty

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef v s) = _def <+> pretty v <> _colon <+> pretty s <> line

instance Ann a => Pretty (VarDefs a) where
   pretty ds = vsep (toList (pretty <$> ds))

instance Ann a => Pretty (Clause a) where
   pretty (Clause (ps × e)) = lambda (toList ps) e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) = pretty (head cs)

instance Ann a => Pretty (RecDefs a) where
   pretty bs = vsep (toList (pretty <$> bs))

instance Ann a => Pretty (Branch a) where
   pretty (v × Clause (ps × e)) =
      _def
         <+> text v
         <> parens (prettyParams (toList ps))
         <> block (pretty e)
         <> line

instance Ann a => Pretty (List (DictEntry a × Expr a)) where
   pretty Nil = mempty
   pretty (kv : Nil) = pretty kv
   pretty (kv : kvs) = pretty kv <> _comma <+> pretty kvs

instance Ann a => Pretty (DictEntry a × Expr a) where
   pretty (k × v) = pretty k <> _colon <+> pretty v

instance Ann a => Pretty (DictEntry a) where
   pretty (ExprKey k) = pretty k
   pretty (VarKey _ k) = text k

prettyCtr :: Ctr -> Doc
prettyCtr = showCtr >>> text

prettyConstr :: forall d. Pretty d => Ctr -> List d -> Doc
prettyConstr c (x : y : ys)
   | c == cPair = assert (null ys) (parens (pretty x <> _comma <+> pretty y))
prettyConstr c ys
   | c == cNil = assert (null ys) (_empty)
prettyConstr c (x : y : ys)
   | c == cCons = assert (null ys) $ brackets (pretty x <> _comma <+> _asterisk <> pretty y)
prettyConstr c (x : Nil) = prettyCtr c <+> pretty x
prettyConstr c xs = hsep (prettyCtr c : (pretty <$> xs))

prettyPattConstr :: Doc -> List Pattern -> Doc
prettyPattConstr _ Nil = Empty
prettyPattConstr _ (Cons p Nil) = pretty p
prettyPattConstr sep (Cons p ps) = pretty p <+> sep <+> prettyPattConstr sep ps

defMatchCase' :: forall a. Ann a => (Pattern × Expr a) -> Doc
defMatchCase' (p × e) = _case <+> (pretty p) <> block (pretty e)

prettyAppChain :: forall a. Ann a => Expr a -> List Doc -> Doc
prettyAppChain (App _ f a) as = prettyAppChain f (pretty a : as)
prettyAppChain f as = pretty f <> parens (hsepWith (text ", ") as)

prettyParams :: forall a. Pretty a => List a -> Doc
prettyParams Nil = mempty
prettyParams (d : Nil) = pretty d
prettyParams (d : ds) = pretty d <> _comma <+> prettyParams ds
