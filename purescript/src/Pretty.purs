module Pretty where

import Prelude
import Data.Array (length, range, take, zipWith)
import Data.Foldable (class Foldable, foldl, foldMap, intercalate)
import Data.Newtype (ala, class Newtype, wrap)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Unfoldable (replicate)
import Expr (Elim(..), Expr, RawExpr(..))
import Eval (ExplVal(..))
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Primitive (BinaryOp, opName)
import Util (error)
import Val (Val)
import Val (Val(..)) as V


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
hcat :: forall f. Foldable f => f Doc -> Doc
hcat = ala Columns foldMap

-- | Stack documents vertically
vcat :: forall f. Foldable f => f Doc -> Doc
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

instance exprPrettyList :: PrettyList RawExpr where
   prettyList (Nil) = text ""
   prettyList (Cons e e') = text ", " :<>: pretty e :<>: prettyList e'
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList Val where
   prettyList (V.Nil) = text ""
   prettyList (V.Cons v v') = text ", " :<>: pretty v :<>: prettyList v'
   prettyList _ = error "Ill-formed list"

instance binaryOpPretty :: Pretty BinaryOp where
   pretty = opName >>> text

instance rawExprPretty :: Pretty RawExpr where
    pretty (Int n) = text (show n)
    pretty (Var x) = text x
    pretty True    = text "true"
    pretty False   = text "false"
    pretty (Pair e1 e2) = text "(" :<>: pretty e1 :<>: text ", " :<>: pretty e2 :<>: text ")"
    pretty Nil = text "[]"
    pretty (Cons e es) = text "[" :<>: pretty e :<>: foldExprCons es :<>: text "]"
    pretty (Let x e1 e2) = atop (text ("let " <> x <> " = ") :<>: pretty e1 :<>: text " in")
                                (pretty e2)
    pretty (Match e σ) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty σ)) (text "}")
    pretty (Letrec x σ e) = atop (text ("letrec " <>  x <> " = ") :<>: pretty σ)
                                        (text "in     " :<>: pretty e)
    pretty (App e1 e2) = pretty e1 :<>: text " " :<>: pretty e2
    pretty (BinaryApp op e1 e2) = pretty e1 :<>: text " " :<>: pretty op :<>: text " " :<>: pretty e2

instance exprElim :: Pretty Elim where
    pretty (ElimVar { x, e }) = text "  " :<>: text x :<>: text " -> " :<>: pretty e
    pretty (ElimPair { x, y, e }) = text "   (" :<>: text x :<>: text ", " :<>: text y :<>: text ") -> " :<>: pretty e
    pretty (ElimList { bnil: e, bcons: { x, y, e: e' } }) = text "    " :<>: atop (text "[] -> " :<>: pretty e) (text "(" :<>: text x :<>: text ":" :<>: text y :<>: text ") -> " :<>: pretty e')
    pretty (ElimBool { btrue: e, bfalse: e' }) = text "     " :<>: atop (text "true -> " :<>: pretty e) (text "false -> " :<>: pretty e')

instance valPretty :: Pretty Val where
    pretty V.Bot = text "⊥"
    pretty (V.Int n)  = text $ show n
    pretty (V.IntSel n)  = text $ show n
    pretty V.True = text "True"
    pretty V.TrueSel = text "True"
    pretty V.False = text "False"
    pretty V.FalseSel = text "False"
    pretty (V.Closure ρ f σ) = text "Closure(" :<>: atop (text "env" :<>: text f ) (pretty σ) :<>: text ")"
    pretty (V.Pair a b) = text "(" :<>: pretty a :<>: text ", " :<>: pretty b :<>: text ")"
    pretty (V.PairSel a b) = text "(" :<>: pretty a :<>: text ", " :<>: pretty b :<>: text ")"
    pretty V.Nil = text "[]"
    pretty V.NilSel = text "[]"
    pretty (V.Cons x xs) = text "[" :<>: pretty x :<>: foldValCons xs :<>: text "]"
    pretty (V.ConsSel x xs) = text "[" :<>: pretty x :<>: foldValCons xs :<>: text "]"
    pretty (V.Failure s) = text s

instance explvalPretty :: Pretty ExplVal where
    pretty (ExplVal {t, v}) = atop (pretty t) (pretty v)

prettyProgram :: Expr -> Doc
prettyProgram e = atop (pretty e) (text "")
