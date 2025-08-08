module Temp.Pretty (prettyPy) where

import Prelude

import Bind (key)
import Data.List (List(..), null, reverse, uncons, (:))
import Data.List.NonEmpty (NonEmptyList, groupBy, head, length, toList, unzip)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), RecDefs, VarDef(..), VarDefs)
import Temp.Pretty.Constants (_case, _colon, _comma, _def, _else, _empty, _equal, _if, _lambda, _lbracket, _match, _rbracket, _return, _star, xs)
import Temp.Pretty.Doc (Doc(..), line, text, (<++>), (<+>))
import Temp.Pretty.Helpers (block, hsep, hsepWith, parens, quotes', render, text', todo, var, vsep)
import Util (type (×), assert, (×))
import Val (class Ann)

class Pretty p where
   pretty :: p -> Doc

prettyPy :: forall a. Ann a => Expr a -> String
prettyPy x = render (topLevel x)

topLevel :: forall a. Ann a => Expr a -> Doc
topLevel expr = case expr of
   Let ds s -> pretty ds <++> line <> topLevel s
   LetRec h s -> defMatchAll h <++> topLevel s
   e -> pretty e

-- defOverload :: forall a. Ann a => RecDefs a -> Doc
-- defOverload bs = vsep (toList (map def bs))

def :: forall a. Ann a => Branch a -> Doc
def (v × Clause (ps × e)) =
   _def
      <+> var v
      <> parens (prettyList ps)
      <> block (defBody e)
      <> line

defBody :: forall a. Ann a => Expr a -> Doc
defBody expr = case expr of
   Let ds s -> pretty ds <++> defBody s
   LetRec h s -> defMatchAll h <++> defBody s
   IfElse i t e -> ite defBody i t e
   (MatchAs s cs) -> _match <+> pretty s <> block (pretty cs)
   e -> _return <+> pretty e

binaryApp :: forall a. Ann a => Int -> Expr a -> Doc
binaryApp n (BinaryApp s op s') =
   case getPrec op of
      -1 -> binaryApp 0 s <+> (text ("`" <> op <> "`")) <+> binaryApp 0 s'
      n' -> if n' <= n then parens (binaryApp n' s <+> text op <+> binaryApp n' s') else binaryApp n' s <+> text op <+> binaryApp n' s'
binaryApp _ e = pretty e

lambda :: forall a. Ann a => List Pattern -> Expr a -> Doc
lambda ps e = _lambda <+> (joinWith "," (map pretty ps)) <> _colon <+> pretty e

ite :: forall a. Ann a => (Expr a -> Doc) -> Expr a -> Expr a -> Expr a -> Doc
ite pretty' i t e = _if <+> pretty i <> block (pretty' t) <++> _else <> block (pretty' e)

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = var x
   pretty (Op op) = text op
   pretty (Int _ _ n) = text' n
   pretty (Float _ _ n) = text' n
   pretty (Str _ _ str) = quotes' str
   pretty (Constr _ _ c Nil) = quotes' c -- temp constr as string
   pretty (Constr _ _ c as) = prettyConstr c as
   pretty (Dictionary _ _ _) = todo "Dict"
   pretty (Matrix _ _ _ _ _) = todo "Matrix"
   pretty (Lambda cs) = parens (pretty cs)
   pretty (Project _ _ _) = todo "Project"
   pretty (DProject _ e k) = pretty e <> _lbracket <> pretty k <> _rbracket
   pretty (App _ (Op op) s') = parens (lambda (PVar "x" : Nil) (BinaryApp s' op (Var "x")))
   -- pretty (App _ (Op op) s') = parens (_lambda <+> _x <> _colon <+> pretty s' <+> text op <+> _x)

   pretty (App d s s') = prettyApp (App d s s')
   pretty (BinaryApp s op s') = binaryApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = _match <+> pretty s <> _colon <> block (pretty cs)
   pretty (IfElse i t e) = ite pretty i t e
   pretty (ListEmpty _ _) = _empty
   pretty (ListNonEmpty _ _ e l) = _lbracket <> pretty e <> pretty l
   pretty (ListEnum _ _) = todo "ListEnum"
   pretty (ListComp _ _ _ _) = todo "ListComp"
   pretty (Let ds s) =
      let
         (ps × es) = unzip $ map (\(VarDef p e) -> p × e) ds
      in
         parens (lambda (toList ps) s) <> parens (prettyList es)
   pretty (LetRec h s) = defMatchAll h <++> pretty s

listCase :: List Pattern -> Doc
listCase Nil = Empty
listCase (Cons p Nil) = pretty p
listCase (Cons p (Cons p' Nil)) = pretty p <> _comma <+> _star <> pretty p'
listCase (Cons p ps) = pretty p <> _comma <+> listCase ps

-- listPattern :: List Pattern -> Doc
-- listPattern Nil = Empty

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty pss = vsep (toList (map defMatchCase' pss))

getPrec :: String -> Int
getPrec x = case lookup x opDefs of
   Just y -> y.prec
   Nothing -> -1

instance Pretty Pattern where
   pretty (PVar x) = var x
   pretty (PRecord _) = todo "PRecord"
   pretty (PConstr c Nil) = quotes' c
   pretty (PConstr "Pair" (x : y : Nil)) = parens (pretty x <> text "," <+> pretty y)
   pretty (PConstr c ps) = case uncons ps of
      Just { head: p, tail: Nil } -> text c <+> pretty p
      _ ->
         if c == cPair then parens $ prettyPattConstr (_comma) ps
         else if c == cCons then _lbracket <> listCase ps <> _rbracket
         else parens $ text c <+> prettyPattConstr Empty ps
   pretty (PListEmpty) = _empty
   pretty (PListNonEmpty p l) = _lbracket <> pretty p <> pretty l

instance Ann a => Pretty (ListRest a) where
   pretty (Next _ (Dictionary _ _ _) _) = todo "listrestdict"
   pretty (Next _ s l) = _comma <+> pretty s <+> pretty l
   pretty (End _) = _rbracket

instance Pretty ListRestPattern where
   pretty (PListVar x) = text x
   pretty (PListNext p l) = _comma <+> pretty p <+> pretty l
   pretty PListEnd = _rbracket

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef p (IfElse i t e)) = ite (\br -> pretty p <+> _equal <+> pretty br) i t e

   pretty (VarDef p s) = pretty p <+> _equal <+> pretty s

instance Ann a => Pretty (VarDefs a) where
   pretty ds = vsep (toList (map pretty ds))

instance Ann a => Pretty (Clause a) where
   pretty (Clause (ps × e)) = lambda (toList ps) e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs)
      | length cs == 1 = pretty (head cs)
      | otherwise = todo "lambdas"

prettyCtr :: Ctr -> Doc
prettyCtr = showCtr >>> text

prettyConstr :: forall d. Pretty d => Ctr -> List d -> Doc
prettyConstr c (x : y : ys)
   | c == cPair = assert (null ys) $ parens (pretty x <> _comma <+> pretty y)
prettyConstr c ys
   | c == cNil = assert (null ys) (_empty)
prettyConstr c (x : y : ys)
   | c == cCons = assert (null ys) $ parens (pretty x <+> _colon <+> pretty y)
prettyConstr c (x : Nil) = prettyCtr c <+> pretty x
prettyConstr c xs = hsep (prettyCtr c : (pretty <$> xs))

prettyPattConstr :: Doc -> List Pattern -> Doc
prettyPattConstr _ Nil = Empty
prettyPattConstr _ (Cons p Nil) = pretty p
prettyPattConstr sep (Cons p ps) = pretty p <+> sep <+> prettyPattConstr sep ps

defMatchAll :: forall a. Ann a => RecDefs a -> Doc
defMatchAll bs = defMatchAll' (groupBy (\p q -> key p == key q) bs)

defMatchAll' :: forall a. Ann a => NonEmptyList (NonEmptyList (Branch a)) -> Doc
defMatchAll' bs = vsep (toList (map defMatchOne bs))

numPats :: forall a. Ann a => Branch a -> Int
numPats (_ × Clause (ps × _)) = length ps

defMatchOne :: forall a. Ann a => NonEmptyList (Branch a) -> Doc
defMatchOne bs =
   if length bs == 1 then def (head bs)
   else
      _def
         <+> var (fst $ head bs)
         <> parens vars'
         <> block
            ( _match
                 <+> vars'
                 <> block (vsep (toList (map defMatchCase bs)))
            )

   where
   vars' = joinWith ", " (xs (numPats $ head bs))

defMatchCase :: forall a. Ann a => Branch a -> Doc
defMatchCase (_ × Clause (ps × e)) = _case <+> (prettyList ps) <> block (defBody e) <> line

defMatchCase' :: forall a. Ann a => (Pattern × Expr a) -> Doc
defMatchCase' (p × e) = _case <+> (pretty p) <> block (defBody e) <> line

prettyApp :: forall a. Ann a => Expr a -> Doc
prettyApp x = prettyAppChain (reverse $ flattenAppChain x)

flattenAppChain :: forall a. Ann a => Expr a -> List Doc
flattenAppChain (App _ s s') = pretty s' : flattenAppChain s
flattenAppChain s = pretty s : Nil

prettyAppChain :: List Doc -> Doc
prettyAppChain Nil = text "hello"
prettyAppChain (x : xs) = x <> parens (joinWith ", " xs)

prettyMap :: forall a. Pretty a => String -> NonEmptyList a -> Doc
prettyMap delim = joinWith delim <<< map pretty <<< toList

joinWith :: String -> List Doc -> Doc
joinWith delim = hsepWith (text delim)

prettyList :: forall a. Pretty a => NonEmptyList a -> Doc
prettyList = prettyMap ", "
