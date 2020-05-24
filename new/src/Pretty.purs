module Pretty where

import Prelude
import Data.Array (length, range, take, zipWith)
import Data.Foldable (class Foldable, foldl, foldMap, intercalate)
import Data.Newtype (ala, class Newtype, wrap)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Unfoldable (replicate)
import Expr (Def(..), Elim(..), Expr(..), RawExpr(..))
import Primitive (BinaryOp(..))
import Util (error)
import Val (RawVal)
import Val (RawVal(..)) as V


-- | A text document.
newtype Doc = Doc
   { width  :: Int
   , height :: Int
   , lines  :: Array String
   }

instance docShow :: Show Doc where
   show = render

-- | Render a document to a string.
render :: Doc -> String
render (Doc doc) = intercalate "\n" doc.lines

-- | An empty document
empty :: Int -> Int -> Doc
empty w h =
   Doc { width: w
      , height: h
      , lines: case h of
                 0 -> []
                 _ -> range 1 h $> ""
      }

-- | Create a document from some text.
text :: String -> Doc
text s =
   Doc { width:  foldl max 0 $ map S.length lines
      , height: length lines
      , lines:  lines
      }
   where
   lines = S.split (wrap "\n") s

-- | Place one document beside another.
beside :: Doc -> Doc -> Doc
beside (Doc d1) (Doc d2) =
   Doc { width:  d1.width + d2.width
      , height: height_
      , lines:  take height_ $ zipWith append (map (padRight d1.width) (adjust d1)) (adjust d2)
      }
   where
   height_ :: Int
   height_ = max d1.height d2.height

   -- Adjust a document to fit the new width and height
   adjust :: { lines :: Array String, width :: Int, height :: Int } -> Array String
   adjust d = d.lines <> replicate (height_ - d.height) (emptyLine d.width)

   emptyLine :: Int -> String
   emptyLine w = SCU.fromCharArray (replicate w ' ' :: Array Char)

   padRight :: Int -> String -> String
   padRight w s = s <> emptyLine (w - S.length s)

infixl 5 beside as :<>:

-- | Place one document on top of another.
atop :: Doc -> Doc -> Doc
atop (Doc d1) (Doc d2) =
   Doc { width:  max d1.width d2.width
      , height: d1.height + d2.height
      , lines:  d1.lines <> d2.lines
      }

-- | Place documents in columns
hcat :: ∀ f. Foldable f => f Doc -> Doc
hcat = ala Columns foldMap

-- | Stack documents vertically
vcat :: ∀ f. Foldable f => f Doc -> Doc
vcat = ala Stack foldMap

-- | A wrapper for `Doc` with a `Monoid` instance which stacks documents vertically.
newtype Stack = Stack Doc

derive instance newtypeStack :: Newtype Stack _

instance semigroupStack :: Semigroup Stack where
   append (Stack d1) (Stack d2) = Stack (d1 `atop` d2)

instance monoidStack :: Monoid Stack where
   mempty = Stack (empty 0 0)

-- | A wrapper for `Doc` with a `Monoid` instance which stacks documents in columns.
newtype Columns = Columns Doc

derive instance newtypeColumns :: Newtype Columns _

instance semigroupColumns :: Semigroup Columns where
   append (Columns d1) (Columns d2) = Columns (d1 `beside` d2)

instance monoidColumns :: Monoid Columns where
   mempty = Columns (empty 0 0)

class Pretty p where
   pretty :: p -> Doc

class PrettyList p where
   prettyList :: p -> Doc

instance exprPrettyList :: PrettyList Expr where
   prettyList (Expr _ r) = prettyList r

instance rawExprPrettyList :: PrettyList RawExpr where
   prettyList (Nil) = text ""
   prettyList (Cons e e') = text ", " :<>: pretty e :<>: prettyList e'
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList RawVal where
   prettyList (V.Nil) = text ""
   prettyList (V.Cons { u } { u: u'}) = text ", " :<>: pretty u :<>: prettyList u'
   prettyList _ = error "Ill-formed list"

instance exprPretty :: Pretty Expr where
   pretty (Expr _ r) = pretty r

instance rawExprPretty :: Pretty RawExpr where
   pretty (Int n) = text (show n)
   pretty (Var x) = text x
   pretty True = text "true"
   pretty False = text "false"
   pretty (Pair e e') = parens (pretty e :<>: text ", " :<>: pretty e')
   pretty Nil = text "[]"
   pretty (Op op) = parens $ text op
   pretty (Cons e e') = text "[" :<>: pretty e :<>: prettyList e' :<>: text "]"
   pretty (Let x e e') =
      atop (text ("let " <> x <> " = ") :<>: pretty e :<>: text " in") (pretty e')
   pretty (Match e σ) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty σ)) (text "}")
   pretty (Letrec (Def f σ) e) =
      atop (text ("letrec " <> f <> " = ") :<>: pretty σ) (text "in     " :<>: pretty e)
   pretty (App e e') = pretty e :<>: text " " :<>: pretty e'
   pretty (BinaryApp e op e') = pretty e :<>: text (" " <> op <> " ") :<>: pretty e'

instance prettyElim :: Pretty k => Pretty (Elim k) where
   pretty (ElimVar x κ) = text "  " :<>: text x :<>: text " -> " :<>: pretty κ
   pretty (ElimPair σ) = text "   " :<>: pretty σ
   pretty (ElimList { nil: κ, cons: σ }) =
      text "    " :<>: atop (text "[] -> " :<>: pretty κ) (pretty σ)
   pretty (ElimBool { true: κ, false: κ' }) =
      text "     " :<>: atop (text "true -> " :<>: pretty κ) (text "false -> " :<>: pretty κ')

instance valPretty :: Pretty RawVal where
   pretty (V.Int n)  = text $ show n
   pretty V.True = text "True"
   pretty V.False = text "False"
   pretty (V.Closure ρ δ σ) = text "Closure" :<>: parens (atop (text "env, defs") (pretty σ))
   pretty (V.Op (BinaryOp { name })) = parens $ text name
   pretty (V.PartialApp (BinaryOp { name }) { u }) = parens $ text (name <> " ") :<>: pretty u
   pretty (V.Pair { u } { u: u' }) = parens $ pretty u :<>: text ", " :<>: pretty u'
   pretty V.Nil = text "[]"
   pretty (V.Cons { u } { u: u' }) = text "[" :<>: pretty u :<>: prettyList u' :<>: text "]"

parens :: Doc -> Doc
parens doc = text "(" :<>: doc :<>: text ")"

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
