module Pretty (class Pretty, pretty, module P) where

import Prelude
import Data.List (List(..), (:))
import Text.Pretty (Doc, atop, beside, text)
import Text.Pretty (render) as P
import Expr (Def(..), Elim(..), Expr(..), RawExpr, RecDef(..))
import Expr (RawExpr(..)) as E
import Parse (cFalse, cNil, cTrue)
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

instance unitPretty :: Pretty Unit where
   pretty _ = text ""

instance rawExprPretty :: Pretty RawExpr where
   pretty (E.Int n) = text $ show n
   pretty (E.Str str) = text $ show str
   pretty (E.Var x) = text x
   pretty E.True = text cTrue
   pretty E.False = text cFalse
   pretty (E.Pair e e') = parens (pretty e :<>: text ", " :<>: pretty e')
   pretty E.Nil = text cNil
   pretty (E.Cons e e') = text "[" :<>: pretty e :<>: prettyList e' :<>: text "]"
   pretty (E.Op op) = parens $ text op
   pretty (E.Let (Def σ e) e') =
      atop (text ("let ") :<>: pretty σ :<>: text " = " :<>: pretty e :<>: text " in") (pretty e')
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
   pretty V.True = text cTrue
   pretty V.False = text cFalse
   pretty (V.Closure ρ δ σ) = text "Closure" :<>: parens (atop (text "env, defs") (pretty σ))
   pretty (V.Unary op) = parens $ pretty op
   pretty (V.Binary op) = parens $ pretty op
   pretty (V.Pair v v') = parens $ pretty v :<>: text ", " :<>: pretty v'
   pretty V.Nil = text cNil
   pretty (V.Cons v v') = text "[" :<>: pretty v :<>: prettyList v' :<>: text "]"

instance unaryOpPretty :: Pretty UnaryOp where
   pretty (UnaryOp name _) = text name
   pretty (PartialApp φ v) = pretty φ :<>: text "-" :<>: pretty v

instance binaryOpPretty :: Pretty BinaryOp where
   pretty (BinaryOp name _) = text name

parens :: Doc -> Doc
parens doc = text "(" :<>: doc :<>: text ")"

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
