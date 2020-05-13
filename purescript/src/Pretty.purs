module Pretty where

import Prelude
import Data.Array (length, range, take, zipWith)
import Data.Foldable (class Foldable, foldl, foldMap, intercalate)
import Data.Newtype (ala, class Newtype, wrap)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Unfoldable (replicate)
import Expr (Elim(..), Expr, RawExpr(..))
import Primitive (BinaryOp, opName)
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
   prettyList (Cons { r } { r: r' }) = text ", " :<>: pretty r :<>: prettyList r'
   prettyList _ = error "Ill-formed list"

instance valPrettyList :: PrettyList RawVal where
   prettyList (V.Nil) = text ""
   prettyList (V.Cons { u } { u: u'}) = text ", " :<>: pretty u :<>: prettyList u'
   prettyList _ = error "Ill-formed list"

instance binaryOpPretty :: Pretty BinaryOp where
   pretty = opName >>> text

instance rawExprPretty :: Pretty RawExpr where
   pretty (Int n) = text (show n)
   pretty (Var x) = text x
   pretty True    = text "true"
   pretty False   = text "false"
   pretty (Pair { r } { r: r' }) = text "(" :<>: pretty r :<>: text ", " :<>: pretty r' :<>: text ")"
   pretty Nil = text "[]"
   pretty (Op op) = text "(" :<>: text (opName op) :<>: text ")"
   pretty (Cons { r } { r: r' }) = text "[" :<>: pretty r :<>: prettyList r' :<>: text "]"

   pretty (Let x { r } { r: r' }) =
      atop (text ("let " <> x <> " = ") :<>: pretty r :<>: text " in") (pretty r')
   pretty (Match { r } σ) = atop (atop (text "match " :<>: pretty r :<>: text " as {") (pretty σ)) (text "}")
   pretty (Letrec x σ { r }) =
      atop (text ("letrec " <>  x <> " = ") :<>: pretty σ) (text "in     " :<>: pretty r)
   pretty (App { r } { r: r' }) = pretty r :<>: text " " :<>: pretty r'
   pretty (BinaryApp op { r } { r: r' }) = pretty r :<>: text " " :<>: pretty op :<>: text " " :<>: pretty r'

instance exprElim :: Pretty Elim where
   pretty (ElimVar { x, e: { r } }) = text "  " :<>: text x :<>: text " -> " :<>: pretty r
   pretty (ElimPair { x, y, e: { r } }) =
      text "   (" :<>: text x :<>: text ", " :<>: text y :<>: text ") -> " :<>: pretty r
   pretty (ElimList { nil: { r }, cons: { x, y, e: { r: r' } } }) =
      text "    " :<>: atop (text "[] -> " :<>: pretty r) (text "(" :<>: text x :<>: text ":" :<>: text y :<>: text ") -> " :<>: pretty r')
   pretty (ElimBool { true: { r }, false: { r: r' } }) =
      text "     " :<>: atop (text "true -> " :<>: pretty r) (text "false -> " :<>: pretty r')

instance valPretty :: Pretty RawVal where
   pretty (V.Int n)  = text $ show n
   pretty V.True = text "True"
   pretty V.False = text "False"
   pretty (V.Closure ρ f σ) = text "Closure(" :<>: atop (text "env" :<>: text f ) (pretty σ) :<>: text ")"
   pretty (V.Op op) = text $ opName op
   pretty (V.Pair { u } { u: u' }) = text "(" :<>: pretty u :<>: text ", " :<>: pretty u' :<>: text ")"
   pretty V.Nil = text "[]"
   pretty (V.Cons { u } { u: u' }) = text "[" :<>: pretty u :<>: prettyList u' :<>: text "]"

prettyProgram :: Expr -> Doc
prettyProgram { r } = atop (pretty r) (text "")
