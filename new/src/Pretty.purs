module Pretty (class Pretty, pretty, module P) where

import Prelude
import Data.List (List(..), (:))
import Text.Pretty (Doc, atop, beside, text)
import Text.Pretty (render) as P
import Expr (Def(..), Elim(..), Expr(..), RawExpr, RecDef(..))
import Expr (RawExpr(..)) as E
import Util (error)
import Val (BinaryOp(..), Val(..), RawVal, UnaryOp(..))
import Val (RawVal(..)) as V

infixl 5 beside as :<>:

class Pretty p where
   pretty :: p -> Doc

class PrettyList p where
   prettyList :: p -> Doc

instance exprPrettyList :: PrettyList Expr where
   prettyList (Expr _ r) = prettyList r

instance rawExprPrettyList :: PrettyList RawExpr where
   prettyList (E.Nil) = text ""
   prettyList (E.Cons e e') = text ", " :<>: pretty e :<>: prettyList e'
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList Val where
   prettyList (Val _ u) = prettyList u

instance rawValPrettyList :: PrettyList RawVal where
   prettyList (V.Nil) = text ""
   prettyList (V.Cons v v') = text ", " :<>: pretty v :<>: prettyList v'
   prettyList _ = error "Ill-formed list"

instance exprPretty :: Pretty Expr where
   pretty (Expr _ r) = pretty r

instance rawExprPretty :: Pretty RawExpr where
   pretty (E.Int n) = text $ show n
   pretty (E.Str str) = text $ show str
   pretty (E.Var x) = text x
   pretty E.True = text "true"
   pretty E.False = text "false"
   pretty (E.Pair e e') = parens (pretty e :<>: text ", " :<>: pretty e')
   pretty E.Nil = text "[]"
   pretty (E.Cons e e') = text "[" :<>: pretty e :<>: prettyList e' :<>: text "]"
   pretty (E.Op op) = parens $ text op
   pretty (E.Let (Def x e) e') =
      atop (text ("let " <> x <> " = ") :<>: pretty e :<>: text " in") (pretty e')
   pretty (E.MatchAs e σ) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty σ)) (text "}")
   pretty (E.LetRec δ e) =
      atop (text "let " :<>: pretty δ) (text "in     " :<>: pretty e)
   pretty (E.Lambda σ) = text "fun" :<>: pretty σ
   pretty (E.App e e') = pretty e :<>: text " " :<>: pretty e'
   pretty (E.BinaryApp e op e') = pretty e :<>: text (" " <> op <> " ") :<>: pretty e'

instance prettyDefs :: Pretty (List RecDef) where
   pretty Nil = text ""
   pretty (RecDef f σ : δ) = atop (text (f <> " = ") :<>: pretty σ) $ pretty δ

instance prettyElim :: Pretty k => Pretty (Elim k) where
   pretty (ElimVar x κ) = text "  " :<>: text x :<>: text " -> " :<>: pretty κ
   pretty (ElimPair σ) = text "   " :<>: pretty σ
   pretty (ElimList { nil: κ, cons: σ }) =
      text "    " :<>: atop (text "[] -> " :<>: pretty κ) (pretty σ)
   pretty (ElimBool { true: κ, false: κ' }) =
      text "     " :<>: atop (text "true -> " :<>: pretty κ) (text "false -> " :<>: pretty κ')

instance valPretty :: Pretty Val where
   pretty (Val _ u) = pretty u

instance rawValPretty :: Pretty RawVal where
   pretty (V.Int n)  = text $ show n
   pretty (V.Str str) = text $ show str
   pretty V.True = text "True"
   pretty V.False = text "False"
   pretty (V.Closure ρ δ σ) = text "Closure" :<>: parens (atop (text "env, defs") (pretty σ))
   pretty (V.Unary (UnaryOp name _)) = parens $ text name
   pretty (V.Binary (BinaryOp name _)) = parens $ text name
   pretty (V.PartialApp (BinaryOp name _) v) = parens $ text (name <> " ") :<>: pretty v
   pretty (V.Pair v v') = parens $ pretty v :<>: text ", " :<>: pretty v'
   pretty V.Nil = text "[]"
   pretty (V.Cons v v') = text "[" :<>: pretty v :<>: prettyList v' :<>: text "]"

parens :: Doc -> Doc
parens doc = text "(" :<>: doc :<>: text ")"

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
