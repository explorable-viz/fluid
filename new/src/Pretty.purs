module Pretty (class Pretty, pretty, module P) where

import Prelude hiding (absurd, between)
import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.String (Pattern(..), contains)
import Text.Pretty (Doc, atop, beside, hcat, render, text, vcat)
import Text.Pretty (render) as P
import DataType (Ctr, cCons, cNil, cPair)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr, RecDef(..), VarDef(..), expr, varAnon)
import Expr (RawExpr(..), Expr(Hole)) as E
import Expl (Expl(..), VarDef(..)) as T
import Expl (Expl, Match(..))
import Util (Endo, type (×), (×), absurd, error, intersperse)
import Val (Binding, Env(..), Primitive(..), RawVal, Val(..), (:+:), (↦), val)
import Val (RawVal(..), Val(Hole)) as V

infixl 5 beside as :<>:

between :: Doc -> Doc -> Endo Doc
between l r doc = l :<>: doc :<>: r

brackets :: Endo Doc
brackets = between (text "[") (text "]")

comma :: Doc
comma = text "," :<>: space

space :: Doc
space = text " "

tab :: Doc
tab = text "   "

operator :: String -> Doc
operator = text >>> between space space

parens :: Endo Doc
parens = between (text "(") (text ")")

null :: Doc
null = text ""

hole :: Doc
hole = text "□"

class ToList a where
   toList :: a -> List a

instance toListExpr :: ToList (Expr Boolean)  where
   toList (Expr _ (E.Constr c (e : e' : Nil))) | c == cCons = e : toList e'
   toList (Expr _ (E.Constr c Nil)) | c == cNil             = Nil
   toList _                                                 = error "not a list"

instance toListVal :: ToList (Val Boolean)  where
   toList (Val _ (V.Constr c (v : v' : Nil))) | c == cCons  = v : toList v'
   toList (Val _ (V.Constr c Nil)) | c == cNil              = Nil
   toList _                                                 = error "not a list"

class Pretty p where
   pretty :: p -> Doc

instance prettyBool :: Pretty Boolean where
   pretty = text <<< show

instance prettyEnv :: Pretty (Env Boolean) where
   pretty (ρ :+: kv) = brackets $ pretty $ ρ :+: kv
   pretty Empty = text "[]"

instance prettyVoid :: Pretty Void where
   pretty _ = error absurd

instance prettyExpl :: Pretty (Expl Boolean) where
   pretty T.Hole                          = hole
   pretty (T.Var x _)                     = text x
   pretty (T.Op op _)                     = text op
   pretty (T.Int n _)                     = text $ show n
   pretty (T.Str s _)                     = text $ show s
   pretty (T.Constr c ts)                 = prettyConstr c ts
   pretty (T.NullConstr c _)              = prettyConstr c (Nil :: List Void)
   pretty (T.Lambda σ)                    = text "fun " :<>: pretty σ
   pretty (T.AppHole t)                   = text "App" :<>: parens (hole :<>: comma :<>: hole)
   pretty (T.App tv t' ξ t'')             =
      text "App" :<>:
      parens (atop (text "t1: " :<>: pretty tv :<>: comma)
                   (atop (text "t2: " :<>: pretty t' :<>: comma)
                         (atop (text "match: " :<>:  pretty ξ :<>: comma) (text "t3: " :<>: pretty t''))))
   pretty (T.AppOp tv tv')                = pretty tv :<>: space :<>: pretty tv'
   pretty (T.BinaryApp tv (op × _) tv')   =
      pretty tv :<>: space :<>: text op :<>: space :<>: pretty tv'
   pretty (T.MatchAs t ξ t')              =
      atop (text "match " :<>: pretty t :<>: text " as {")
           (atop (tab :<>: pretty ξ) (atop (text "} where outcome was: ") (tab :<>: pretty t')))
   pretty (T.Let (T.VarDef ξ t) t')       =
      atop (text "let " :<>: pretty ξ :<>: text " = " :<>: pretty t :<>: text " in")
           (pretty t')
   pretty (T.LetRec δ t)                  =
      atop (text "letrec " :<>: pretty δ)
           (text "in     " :<>: pretty t)

instance prettyMatch :: Pretty (Match Boolean) where
   pretty (MatchConstr (c × ξs) κs) =
      text "ξ = " :<>:
      atop (text "Pattern:       " :<>: text (show c) :<>: operator "-> " :<>: vcat (map pretty ξs))
           (text "Continuations: " :<>: vcat (map pretty $ (toUnfoldable κs :: List _)))
   pretty (MatchVar x) = text "ξ = " :<>: text x
   pretty (MatchVarAnon x) = text "ξ = " :<>: text varAnon

instance prettyExplVal :: Pretty (Expl Boolean × Val Boolean) where
   pretty (t × v) = parens $ pretty t :<>: comma :<>: pretty v

instance prettyList :: Pretty a => Pretty (List a) where
   pretty xs = brackets $ hcat $ intersperse comma $ map pretty xs

instance prettyExpr :: Pretty (Expr Boolean) where
   pretty E.Hole     = hole
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

instance prettyRawExpr :: Pretty (RawExpr Boolean) where
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
      text "match " :<>: pretty e :<>: text " as { " :<>: pretty σ :<>: text "}"
   pretty (E.LetRec δ e)            =
      atop (text "letrec " :<>: pretty δ) (text "in " :<>: pretty e)
   pretty (E.Lambda σ)              = text "fun " :<>: pretty σ
   pretty (E.App e e')              = pretty e :<>: space :<>: pretty e'
   pretty (E.BinaryApp e op e')     = pretty e :<>: operator op :<>: pretty e'

instance prettyRecDef :: Pretty (RecDef Boolean) where
   pretty (RecDef f σ) = text f :<>: operator "=" :<>: pretty σ

instance prettyBinding :: Pretty (Binding Boolean) where
   pretty (x ↦ v) = text x :<>: text " ↦ " :<>: pretty v

instance prettyCont :: Pretty (Cont Boolean) where
   pretty None          = text "[ ]"
   pretty (Body e)      = pretty e
   pretty (Arg σ)       = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont Boolean) where
   pretty (c × κ) = text (show c) :<>: operator "->" :<>: pretty κ

instance prettyElim :: Pretty (Elim Boolean) where
   pretty (ElimVar x κ)    = text x :<>: operator "->" :<>: pretty κ
   pretty (ElimConstr κs)  = hcat $ map (\x -> pretty x :<>: comma) $ (toUnfoldable κs :: List _)

instance prettyVal :: Pretty (Val Boolean) where
   pretty V.Hole     = hole
   pretty (Val _ u)  = pretty u

instance prettyRawVal :: Pretty (RawVal Boolean) where
   pretty (V.Int n)              = text $ show n
   pretty (V.Str str)            = text $ show str
   pretty u@(V.Constr c vs)
      | c == cNil || c == cCons  = pretty $ toList $ val u
      | otherwise                = prettyConstr c vs
   pretty (V.Closure ρ δ σ)      =
    text "Closure" :<>: text "(" :<>:
    (atop (atop (text "env: " :<>: pretty ρ) (text "defs: " :<>: pretty δ)) (text "elim: " :<>: pretty σ)) :<>: (text ")")
   pretty (V.Primitive op)       = parens $ pretty op

instance prettyPrimitive :: Pretty Primitive where
   pretty (IntOp _) = text "<prim-op>"

prettyProgram :: Expr Boolean -> Doc
prettyProgram e = atop (pretty e) (text "")
