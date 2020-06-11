module Pretty (class Pretty, pretty, module P) where

import Prelude
import Data.List (List(..), (:))
import Text.Pretty (Doc, atop, beside, hcat, text)
import Text.Pretty (render) as P
import DataType (Ctr(..))
import Elim (Elim(..))
import Expr (Def(..), Expr(..), RawExpr, RecDef(..))
import Expr (RawExpr(..)) as E
import Parse (cFalse, cNil, cTrue)
import Util (error, intersperse)
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
   prettyList (E.Nil) = null
   prettyList (E.Cons e e') = comma :<>: pretty e :<>: prettyList e'
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList Val where
   prettyList (Val _ u) = prettyList u

instance rawValPrettyList :: PrettyList RawVal where
   prettyList (V.Nil) = null
   prettyList (V.Cons v v') = comma :<>: pretty v :<>: prettyList v'
   prettyList _ = error "Ill-formed list"

instance exprPretty :: Pretty Expr where
   pretty (Expr _ r) = pretty r

instance unitPretty :: Pretty Unit where
   pretty _ = null

instance rawExprPretty :: Pretty RawExpr where
   pretty (E.Int n) = text $ show n
   pretty (E.Str str) = text $ show str
   pretty (E.Var x) = text x
   pretty E.True = text cTrue
   pretty E.False = text cFalse
   pretty (E.Pair e e') = parens $ pretty e :<>: comma :<>: pretty e'
   pretty E.Nil = text cNil
   pretty (E.Cons e e') = brackets $ pretty e :<>: prettyList e'
   pretty (E.Op op) = parens $ text op
   pretty (E.Let (Def σ e) e') =
      atop (text ("let ") :<>: pretty σ :<>: text " = " :<>: pretty e :<>: text " in") (pretty e')
   pretty (E.MatchAs e σ) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty σ)) (text "}")
   pretty (E.LetRec δ e) =
      atop (text "let " :<>: pretty δ) (text "in " :<>: pretty e)
   pretty (E.Lambda σ) = text "fun" :<>: pretty σ
   pretty (E.App e e') = pretty e :<>: space :<>: pretty e'
   pretty (E.BinaryApp e op e') = pretty e :<>: operator op :<>: pretty e'

instance prettyDefs :: Pretty (List RecDef) where
   pretty Nil = text ""
   pretty (RecDef f σ : δ) = atop (text f :<>: operator "=" :<>: pretty σ) $ pretty δ

instance prettyElim :: Pretty k => Pretty (Elim k) where
   pretty (ElimVar x κ) = text x :<>: operator "->" :<>: pretty κ
   pretty (ElimPair σ) = pretty σ
   pretty (ElimList { nil: κ, cons: σ }) =
      atop (text "[]" :<>: operator "->" :<>: pretty κ) (text "Cons" :<>: operator "->" :<>: pretty σ)
   pretty (ElimBool { true: κ, false: κ' }) =
      atop (text "true" :<>: operator "->" :<>: pretty κ) (text "false" :<>: operator "->" :<>: pretty κ')

instance valPretty :: Pretty Val where
   pretty (Val _ u) = pretty u

instance rawValPretty :: Pretty RawVal where
   pretty (V.Int n)  = text $ show n
   pretty (V.Str str) = text $ show str
   pretty (V.Constr (Ctr c _) vs) = hcat $ intersperse space $ map pretty vs
   pretty V.True = text cTrue
   pretty V.False = text cFalse
   pretty (V.Closure ρ δ σ) = text "Closure" :<>: parens (atop (text "env, defs") (pretty σ))
   pretty (V.Unary op) = parens $ pretty op
   pretty (V.Binary op) = parens $ pretty op
   pretty (V.Pair v v') = parens $ pretty v :<>: comma :<>: pretty v'
   pretty V.Nil = text cNil
   pretty (V.Cons v v') = brackets $ pretty v :<>: prettyList v'

instance unaryOpPretty :: Pretty UnaryOp where
   pretty (UnaryOp name _) = text name
   pretty (PartialApp φ v) = pretty φ :<>: text "-" :<>: pretty v

instance binaryOpPretty :: Pretty BinaryOp where
   pretty (BinaryOp name _) = text name

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
