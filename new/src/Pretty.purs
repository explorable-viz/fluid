module Pretty (class Pretty, pretty, module P) where

import Prelude hiding (absurd)
import Data.List (List(..), (:), head)
import Data.Map (toUnfoldable)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Text.Pretty (Doc, atop, beside, hcat, render, text, vcat)
import Text.Pretty (render) as P
import DataType (Ctr(..), cPair, cCons)
import Expr (Cont(..), Def(..), Elim(..), Expr(..), RawExpr, RecDef(..))
import Expr (RawExpr(..)) as E
import Util (type (×), absurd, error, fromJust, intersperse)
import Val (BinaryOp(..), Val(..), RawVal, UnaryOp(..))
import Val (RawVal(..)) as V

infixl 5 beside as :<>:

brackets :: Doc -> Doc
brackets doc = text "[" :<>: doc :<>: text "]"

comma :: Doc
comma = text "," :<>: space

space :: Doc
space = text " "

operator :: String -> Doc
operator op = space :<>: text op :<>: space

parens :: Doc -> Doc
parens doc = text "(" :<>: doc :<>: text ")"

null :: Doc
null = text ""

class Pretty p where
   pretty :: p -> Doc

class PrettyList p where
   prettyList :: p -> Doc

instance exprPrettyList :: PrettyList Expr where
   prettyList (Expr _ r) = prettyList r

instance rawExprPrettyList :: PrettyList RawExpr where
   prettyList (E.Constr cNil Nil) = null
   prettyList (E.Constr cCons (e:es:Nil)) = comma :<>: pretty e :<>: prettyList es
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList Val where
   prettyList (Val _ u) = prettyList u

instance rawValPrettyList :: PrettyList RawVal where
   prettyList (V.Constr cNil Nil) = null
   prettyList (V.Constr cCons (v:vs:Nil)) = comma :<>: pretty v :<>: prettyList vs
   prettyList _ = error "Ill-formed list"

instance exprPretty :: Pretty Expr where
   pretty (Expr _ r) = pretty r

instance prettyCtr :: Pretty Ctr where
   pretty = show >>> text

-- Cheap hack to make progress on migrating some tests; need to revisit.
prettyParensOpt :: forall a . Pretty a => a -> Doc
prettyParensOpt x =
   let doc = pretty x in
   if contains (Pattern " ") $ render doc
   then parens doc
   else doc


prettyConstr :: forall a . Pretty a => PrettyList a => Ctr -> List a -> Doc
prettyConstr c Nil = pretty c
prettyConstr c xs@(x : xs')
   | c == cPair = parens $ pretty x :<>: comma :<>: pretty (fromJust absurd $ head xs')
   | c == cCons = brackets $ pretty x :<>: prettyList (fromJust absurd $ head xs')
   | otherwise  = pretty c :<>: space :<>: hcat (intersperse space $ map prettyParensOpt xs)

instance rawExprPretty :: Pretty RawExpr where
   pretty (E.Int n) = text $ show n
   pretty (E.Str str) = text $ show str
   pretty (E.Var x) = text x
   pretty (E.Constr c es) = prettyConstr c es
   pretty (E.Op op) = parens $ text op
   pretty (E.Let (Def σ e) e') =
      atop (text ("let ") :<>: pretty σ :<>: operator "=" :<>: pretty e :<>: text " in") (pretty e')
   pretty (E.MatchAs e σ) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty σ)) (text "}")
   pretty (E.LetRec δ e) =
      atop (text "let " :<>: pretty δ) (text "in " :<>: pretty e)
   pretty (E.Lambda σ) = text "fun" :<>: pretty σ
   pretty (E.App e e') = pretty e :<>: space :<>: pretty e'
   pretty (E.BinaryApp e op e') = pretty e :<>: operator op :<>: pretty e'

instance prettyDefs :: Pretty (List RecDef) where
   pretty Nil = text ""
   pretty (RecDef f σ : δ) = atop (text f :<>: operator "=" :<>: pretty σ) $ pretty δ

instance prettyCont :: Pretty Cont where
   pretty None = text "[ ]"
   pretty (Body e) = pretty e
   pretty (Arg _ σ) = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont) where
   pretty (Tuple c κ) = text (show c) :<>: operator "->" :<>: pretty κ

instance prettyElim2 :: Pretty Elim where
   pretty (ElimVar x κ) = text x :<>: operator "->" :<>: pretty κ
   pretty (ElimConstr κs) = vcat $ map pretty $ (toUnfoldable κs :: List _)

instance valPretty :: Pretty Val where
   pretty (Val _ u) = pretty u

instance rawValPretty :: Pretty RawVal where
   pretty (V.Int n)  = text $ show n
   pretty (V.Str str) = text $ show str
   pretty (V.Constr c vs) = prettyConstr c vs
   pretty (V.Closure ρ δ σ) = text "Closure" :<>: parens (atop (text "env, defs") (pretty σ))
   pretty (V.Unary op) = parens $ pretty op
   pretty (V.Binary op) = parens $ pretty op

instance unaryOpPretty :: Pretty UnaryOp where
   pretty (UnaryOp name _) = text name
   pretty (PartialApp φ v) = pretty φ :<>: text "-" :<>: pretty v

instance binaryOpPretty :: Pretty BinaryOp where
   pretty (BinaryOp name _) = text name

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
