module Pretty2 (class Pretty2, pretty2, module P, (:<>:)) where

import Prelude hiding (absurd)
import Data.List (List(..), (:), head)
import Data.Map (Map, toUnfoldable)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Text.Pretty (Doc, atop, beside, hcat, render, text, vcat)
import Text.Pretty (render) as P
import Bindings (Bindings(..), Bind, (:+:), (↦), elem)
import DataType (Ctr, cPair, cCons)
import Expr (Cont(..), Def(..), Elim(..), Expr(..), RawExpr, RecDef(..))
import Expr (RawExpr(..)) as E
import Expl as T
import Expl (Expl, Match(..))
import Util (type (×), (×), absurd, error, fromJust, intersperse)
import Val (BinaryOp(..), Val(..), RawVal, UnaryOp(..))
import Val (RawVal(..)) as V
import Primitive (primitives)

infixl 5 beside as :<>:

brackets :: Doc -> Doc
brackets doc = text "[" :<>: doc :<>: text "]"

comma :: Doc
comma = text "," :<>: space

space :: Doc
space = text " "

tab :: Doc
tab = text "   "

operator :: String -> Doc
operator op = space :<>: text op :<>: space

parens :: Doc -> Doc
parens doc = text "(" :<>: doc :<>: text ")"

parens2 :: Doc -> Doc
parens2 doc = atop (text "(" :<>: doc)  (text ")")

cbrackets :: Doc -> Doc
cbrackets doc = text "{" :<>: doc :<>: text "}"

null :: Doc
null = text ""

class Pretty2 p where
   pretty2 :: p -> Doc

class PrettyList2 p where
   prettyList2 :: p -> Doc

instance boolPretty :: Pretty2 Boolean where
   pretty2 true = text "true"
   pretty2 false = text "false"

instance envPretty :: Pretty2 (Bindings Val) where
   pretty2 (ρ :+: x ↦ v) = brackets $ prettyList2 ρ :<>: (text x :<>: text " ↦ " :<>: pretty2 v)
   pretty2 Empty = text "[]"



instance explPretty :: Pretty2 Expl where
   pretty2 (T.Var x ρ) = text "Var" :<>: parens (text x)
   pretty2 (T.Op op ρ) = text op
   pretty2 (T.Int n ρ) = text $ show n
   pretty2 (T.Str s) = text s
   pretty2 (T.Constr c es) = prettyConstr c es
   pretty2 (T.NullConstr c ρ) = pretty2 c
   pretty2 (T.Lambda σ) = text "fun" :<>: pretty2 σ
   pretty2 (T.App tv t' ξ t'') =  text "App" :<>: parens (atop (text "t1: " :<>: pretty2 tv :<>: comma) (atop (text "t2: " :<>: pretty2 t' :<>: comma) (atop (text "match: " :<>:  pretty2 ξ :<>: comma) (text "t3: " :<>: pretty2 t''))))
   pretty2 (T.AppOp tv tv') = pretty2 tv :<>: space :<>: pretty2 tv'
   pretty2 (T.BinaryApp tv op tv') = pretty2 tv :<>: space :<>: text op :<>: space :<>: pretty2 tv'
   pretty2 (T.MatchAs t ξ t') = atop (text "match " :<>: pretty2 t :<>: text " as {")
                                 (atop (tab :<>: pretty2 ξ) (atop (text "} where outcome was: ") (tab :<>: pretty2 t') ))
   pretty2 (T.Let (T.Def ξ t) t') = atop (text "let " :<>: pretty2 ξ :<>: text " = " :<>: pretty2 t :<>: text " in")
                                        (pretty2 t')
   pretty2 (T.LetRec δ t) = atop (text "letrec " :<>: pretty2 δ) (text "in     " :<>: pretty2 t)

instance explMatch :: Pretty2 Match where
   pretty2 (MatchConstr (ctr × ξs) ks) = text "ξ = " :<>: (atop (text "Pattern:       " :<>: pretty2 (ctr × ξs))
                                                               (text "Continuations: " :<>: pretty2 ks))
   pretty2 (MatchVar x) = text "ξ = " :<>: text x

instance explValPretty :: Pretty2 (Expl × Val) where
   pretty2 (a × b) = parens $ pretty2 a :<>: comma :<>: pretty2 b

instance envPrettyList :: PrettyList2 (Bindings Val) where
   prettyList2 (ρ :+: x ↦ Val α v)
      | x `elem` primitives = prettyList2 ρ :<>: (text x :<>: text " ↦ " :<>: pretty2 v) :<>: text ", "
      | otherwise           = prettyList2 ρ
   prettyList2 Empty = text ""

instance explPrettyList :: PrettyList2 Expl where
   prettyList2 (T.Constr cNil Nil) = null
   prettyList2 (T.NullConstr cNil ρ) = text "NilExpl"
   prettyList2 (T.Constr cCons (e:es:Nil)) = comma :<>: pretty2 e :<>: prettyList2 es
   prettyList2 t = error "Ill-formed list for expls"

instance exprPrettyList :: PrettyList2 Expr where
   prettyList2 (Expr a r) = text "Expr (" :<>: text (show a) :<>: comma :<>: prettyList2 r :<>: text ")"

instance rawExprPrettyList :: PrettyList2 RawExpr where
   prettyList2 (E.Constr cNil Nil) = null
   prettyList2 (E.Constr cCons (e:es:Nil)) = comma :<>: pretty2 e :<>: prettyList2 es
   prettyList2 e = error "Ill-formed list for exprs"

instance valPrettyList :: PrettyList2 Val where
   prettyList2 (Val _ u) = prettyList2 u

instance rawValPrettyList :: PrettyList2 RawVal where
   prettyList2 (V.Constr cNil Nil) = null
   prettyList2 (V.Constr cCons (v:vs:Nil)) = comma :<>: pretty2 v :<>: prettyList2 vs
   prettyList2 v = error "Ill-formed list for values"

instance exprPretty :: Pretty2 Expr where
   pretty2 (Expr a r) = text "Expr (" :<>: text (show a) :<>: comma :<>: pretty2 r :<>: text ")"

instance prettyCtr :: Pretty2 Ctr where
   pretty2 = show >>> text

-- Cheap hack to make progress on migrating some tests; need to revisit.
prettyParensOpt :: forall a . Pretty2 a => a -> Doc
prettyParensOpt x =
   let doc = pretty2 x in
   if contains (Pattern " ") $ render doc
   then parens doc
   else doc


prettyConstr :: forall a . Pretty2 a => PrettyList2 a => Ctr -> List a -> Doc
prettyConstr c Nil = pretty2 c
prettyConstr c xs@(x : xs')
   | c == cPair = parens $ pretty2 x :<>: comma :<>: pretty2 (fromJust absurd $ head xs')
   | c == cCons = text "Cons (" :<>: pretty2 x :<>: comma :<>: pretty2 (fromJust absurd $ head xs') :<>: text ")"
   | otherwise  = pretty2 c :<>: space :<>: hcat (intersperse space $ map prettyParensOpt xs)

instance rawExprPretty :: Pretty2 RawExpr where
   pretty2 (E.Int n) = text $ show n
   pretty2 (E.Str str) = text $ show str
   pretty2 (E.Var x) = text x
   pretty2 (E.Constr c es) = prettyConstr c es
   pretty2 (E.Op op) = parens $ text op
   pretty2 (E.Let (Def σ e) e') =
      atop (text ("let ") :<>: pretty2 σ :<>: operator "->" :<>: pretty2 e :<>: text " in") (pretty2 e')
   pretty2 (E.MatchAs e σ) = atop (atop (text "match " :<>: pretty2 e :<>: text " as {") (tab :<>: pretty2 σ)) (text "}")
   pretty2 (E.LetRec δ e) =
      atop (text "letrec " :<>: pretty2 δ) (text "in " :<>: pretty2 e)
   pretty2 (E.Lambda σ) = text "fun " :<>: pretty2 σ
   pretty2 (E.App e e') = pretty2 e :<>: space :<>: pretty2 e'
   pretty2 (E.BinaryApp e op e') = pretty2 e :<>: operator op :<>: pretty2 e'

-- instance defPretty :: Pretty2 Def where
--    pretty2 (Def (ElimVar x _) e) = "x" :<>: operator "->" :<>: e
--    pretty2 (Def (ElimConstr m e)) =
--       case toUnfoldable m :: List (Ctr × Cont) of
--          Nil         -> error "Pretty2 printing: absurd"
--          Ctr c × κ   -> text c × pretty2 κ

instance prettylistExpl :: Pretty2 (List Expl) where
   pretty2 Nil    = text ""
   pretty2 (v:vs) = brackets (pretty2 v :<>: prettyList2 vs)

instance prettylistExplList :: PrettyList2 (List Expl) where
   prettyList2 Nil    = text ""
   prettyList2 (v:vs) = comma :<>: pretty2 v :<>: prettyList2 vs

instance prettylistExplVal :: Pretty2 (List (Tuple  Expl Val)) where
   pretty2 Nil    = text ""
   pretty2 (v:vs) = brackets (pretty2 v :<>: prettyList2 vs)

instance prettylistExplValList :: PrettyList2 (List (Tuple  Expl Val)) where
   prettyList2 Nil    = text ""
   prettyList2 (v:vs) = comma :<>: pretty2 v :<>: prettyList2 vs

instance prettyVEB :: Pretty2 (List (Tuple (Bindings Val) (Tuple Expr Boolean))) where
   pretty2 Nil    = text ""
   pretty2 ((v × e × b):vs) = brackets (parens (pretty2 v) :<>: prettyList2 vs)

instance prettyVEBList :: PrettyList2 (List (Tuple (Bindings Val) (Tuple Expr Boolean))) where
   prettyList2 Nil    = text ""
   prettyList2 ((v × e × b):vs) = comma :<>: (parens (pretty2 v) :<>: prettyList2 vs)


instance prettyDefs :: Pretty2 (List RecDef) where
   pretty2 Nil = text ""
   pretty2 (RecDef f σ : δ) = atop (text f :<>: operator "=" :<>: pretty2 σ) $ pretty2 δ

instance prettyMatches :: Pretty2 (List Match) where
   pretty2 Nil    = text ""
   pretty2 (ξ:ξs) = atop (pretty2 ξ) $ pretty2 ξs

instance prettyValsList :: PrettyList2 (List Val) where
   prettyList2 Nil    = text ""
   prettyList2 (v:vs) = comma :<>: pretty2 v :<>: prettyList2 vs

instance prettyVals :: Pretty2 (List Val) where
   pretty2 Nil    = text ""
   pretty2 (v:vs) = brackets (pretty2 v :<>: prettyList2 vs)

instance prettyExprList :: PrettyList2 (List Expr) where
   prettyList2 Nil    = text ""
   prettyList2 (e:es) = comma :<>: pretty2 e :<>: prettyList2 es

instance prettyExpr :: Pretty2 (List Expr) where
   pretty2 Nil    = text ""
   pretty2 (e:es) = brackets (pretty2 e :<>: prettyList2 es)

instance prettyBranches :: Pretty2 (Map Ctr Cont) where
   pretty2 m = vcat $ map pretty2 $ (toUnfoldable m :: List _)

instance prettyBind :: Pretty2 a => Pretty2 (Bind a) where
   pretty2 (x ↦ v) = text x :<>: text " ↦ " :<>: pretty2 v

instance prettyCont :: Pretty2 Cont where
   pretty2 None = text "None"
   pretty2 (Body e) = text "Body (" :<>: pretty2 e :<>: text ")"
   pretty2 (Arg _ σ) = text "Arg (" :<>: pretty2 σ :<>: text ")"

instance prettyBranch :: Pretty2 (Ctr × Cont) where
   pretty2 (Tuple c κ) = text (show c) :<>: operator "->" :<>: pretty2 κ

instance prettyBranch2 :: Pretty2 (Ctr × List Match) where
   pretty2 (Tuple c ξs) = text (show c) :<>: operator "-> " :<>: pretty2 ξs

instance prettyElim2 :: Pretty2 Elim where
   pretty2 (ElimVar x κ) = text "ElimVar " :<>: text x :<>: operator "->" :<>: pretty2 κ
   pretty2 (ElimConstr κs) = text "ElimConstr " :<>: (vcat $ map pretty2 $ (toUnfoldable κs :: List _))

instance valPretty :: Pretty2 Val where
   pretty2 (Val a u) = text "Val (" :<>: text (show a) :<>: comma :<>: pretty2 u :<>: text ")"

instance rawValPretty :: Pretty2 RawVal where
   pretty2 (V.Int n)  = text $ show n
   pretty2 (V.Str str) = text $ show str
   pretty2 (V.Constr c vs) = prettyConstr c vs
   pretty2 (V.Closure ρ δ σ) 
    = text "Closure" :<>: text "(" :<>: 
    (atop (atop (text "env: " :<>: pretty2 ρ) (text "defs: " :<>: pretty2 δ)) (text "elim: " :<>: pretty2 σ)) :<>: (text ")")
   pretty2 (V.Unary op) = parens $ pretty2 op
   pretty2 (V.Binary op) = parens $ pretty2 op

instance unaryOpPretty :: Pretty2 UnaryOp where
   pretty2 (UnaryOp name _) = text name
   pretty2 (PartialApp φ v) = pretty2 φ :<>: text "-" :<>: pretty2 v

instance binaryOpPretty :: Pretty2 BinaryOp where
   pretty2 (BinaryOp name _) = text name

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty2 e) (text "")
