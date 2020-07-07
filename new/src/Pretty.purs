module Pretty (class Pretty, pretty, module P, (:<>:)) where

import Prelude hiding (absurd)
import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.String (Pattern(..), contains)
import Text.Pretty (Doc, atop, beside, hcat, render, text, vcat)
import Text.Pretty (render) as P
import DataType (Ctr, cCons, cNil, cPair)
import Expr (Cont'(..), Elim'(..), Expr'(..), RawExpr', RecDef'(..), VarDef'(..), expr, varAnon)
import Expr (RawExpr'(..)) as E
import Expl (Expl'(..), VarDef'(..)) as T
import Expl (Expl', Match'(..))
import Util (type (×), (×), absurd, error, intersperse, toList)
import Val (Bind', Env'(..), Primitive(..), RawVal', Val'(..), (:+:), (↦), val)
import Val (RawVal'(..)) as V

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

null :: Doc
null = text ""

class Pretty p where
   pretty :: p -> Doc

class PrettyList p where
   prettyList :: p -> Doc

instance boolPretty :: Pretty Boolean where
   pretty = text <<< show

instance envPretty :: Pretty (Env' Boolean) where
   pretty (ρ :+: kv) = brackets $ pretty $ ρ :+: kv
   pretty Empty = text "[]"

instance explPretty :: Pretty (Expl' Boolean) where
   pretty (T.Var x _)               = text x
   pretty (T.Op op _)               = text op
   pretty (T.Int n _)               = text $ show n
   pretty (T.Str s _)               = text $ show s
   pretty (T.Constr c ts)           = prettyConstr c ts
   pretty (T.NullConstr c _)        = pretty c
   pretty (T.Lambda σ)              = text "fun " :<>: pretty σ
   pretty (T.App tv t' ξ t'')       =
      text "App" :<>:
      parens (atop (text "t1: " :<>: pretty tv :<>: comma)
                   (atop (text "t2: " :<>: pretty t' :<>: comma)
                         (atop (text "match: " :<>:  pretty ξ :<>: comma) (text "t3: " :<>: pretty t''))))
   pretty (T.AppOp tv tv')          = pretty tv :<>: space :<>: pretty tv'
   pretty (T.BinaryApp tv op tv')   = pretty tv :<>: space :<>: text op :<>: space :<>: pretty tv'
   pretty (T.MatchAs t ξ t')        =
      atop (text "match " :<>: pretty t :<>: text " as {")
           (atop (tab :<>: pretty ξ) (atop (text "} where outcome was: ") (tab :<>: pretty t')))
   pretty (T.Let (T.VarDef ξ t) t') = atop (text "let " :<>: pretty ξ :<>: text " = " :<>: pretty t :<>: text " in")
                                        (pretty t')
   pretty (T.LetRec δ t)            =
      atop (text "letrec " :<>: pretty δ)
           (text "in     " :<>: pretty t)

instance explMatch :: Pretty (Match' Boolean) where
   pretty (MatchConstr (c × ξs) κs) =
      text "ξ = " :<>:
      atop (text "Pattern:       " :<>: text (show c) :<>: operator "-> " :<>: vcat (map pretty ξs))
           (text "Continuations: " :<>: vcat (map pretty $ (toUnfoldable κs :: List _)))
   pretty (MatchVar x) = text "ξ = " :<>: text x
   pretty (MatchVarAnon x) = text "ξ = " :<>: text varAnon

instance explValPretty :: Pretty (Expl' Boolean × Val' Boolean) where
   pretty (t × v) = parens $ pretty t :<>: comma :<>: pretty v

instance explPrettyList :: PrettyList (Expl' Boolean) where
   prettyList (T.NullConstr c _) | c == cNil             = null
   prettyList (T.Constr c (t : t' : Nil)) | c == cCons   = comma :<>: pretty t :<>: prettyList t'
   prettyList _                                          = error "Not a list"

instance prettyListPretty :: Pretty a => Pretty (List a) where
   pretty xs = brackets $ hcat $ intersperse comma $ map pretty xs

instance exprPretty :: Pretty (Expr' Boolean) where
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

prettyConstr :: forall a . Pretty a => Ctr -> List a -> Doc
prettyConstr c xs
   | c == cPair = case xs of
      x : y : Nil -> parens $ pretty x :<>: comma :<>: pretty y
      _           -> error absurd
   | c == cNil || c == cCons = pretty xs
   | otherwise = pretty c :<>: space :<>: hcat (intersperse space $ map prettyParensOpt xs)

instance rawExprPretty :: Pretty (RawExpr' Boolean) where
   pretty (E.Int n)                 = text $ show n
   pretty (E.Str str)               = text $ show str
   pretty (E.Var x)                 = text x
   pretty r@(E.Constr c es)
      | c == cNil || c == cCons     = pretty $ toList $ expr r
      | otherwise                   = prettyConstr c es
   pretty (E.Op op)                 = parens $ text op
   pretty (E.Let (VarDef σ e) e')   =
      atop (text ("let ") :<>: pretty σ :<>: operator "->" :<>: pretty e :<>: text " in") (pretty e')
   pretty (E.MatchAs e σ)           =
      atop (atop (text "match " :<>: pretty e :<>: text " as {") (tab :<>: pretty σ)) (text "}")
   pretty (E.LetRec δ e)            =
      atop (text "letrec " :<>: pretty δ) (text "in " :<>: pretty e)
   pretty (E.Lambda σ)              = text "fun " :<>: pretty σ
   pretty (E.App e e')              = pretty e :<>: space :<>: pretty e'
   pretty (E.BinaryApp e op e')     = pretty e :<>: operator op :<>: pretty e'

instance prettyRecDef :: Pretty (RecDef' Boolean) where
   pretty (RecDef f σ) = text f :<>: operator "=" :<>: pretty σ

instance prettyBind :: Pretty (Bind' Boolean) where
   pretty (x ↦ v) = text x :<>: text " ↦ " :<>: pretty v

instance prettyCont :: Pretty (Cont' Boolean) where
   pretty None          = text "[ ]"
   pretty (Body e)      = pretty e
   pretty (Arg σ)       = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont' Boolean) where
   pretty (c × κ) = text (show c) :<>: operator "->" :<>: pretty κ

instance prettyElim :: Pretty (Elim' Boolean) where
   pretty (ElimVar x κ)    = text x :<>: operator "->" :<>: pretty κ
   pretty (ElimConstr κs)  = vcat $ map pretty $ (toUnfoldable κs :: List _)

instance valPretty :: Pretty (Val' Boolean) where
   pretty (Val a u) = pretty u

instance rawValPretty :: Pretty (RawVal' Boolean) where
   pretty (V.Int n)           = text $ show n
   pretty (V.Str str)         = text $ show str
   pretty u@(V.Constr c vs)
      | c == cNil || c == cCons     = pretty $ toList $ val u
      | otherwise                   = prettyConstr c vs
   pretty (V.Closure ρ δ σ)   =
    text "Closure" :<>: text "(" :<>:
    (atop (atop (text "env: " :<>: pretty ρ) (text "defs: " :<>: pretty δ)) (text "elim: " :<>: pretty σ)) :<>: (text ")")
   pretty (V.Primitive op)    = parens $ pretty op

instance unaryOpPretty :: Pretty Primitive where
   pretty (IntOp _) = text "<prim-op>"

prettyProgram :: Expr' Boolean -> Doc
prettyProgram e = atop (pretty e) (text "")
