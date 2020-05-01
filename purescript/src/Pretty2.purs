module Pretty2 where

import Expr2 (Elim(..), Expr(..), Typ(..), Val(..))
import Eval2 (ExplVal(..))
import Expl2 (Expl(..), Match(..))
import Prelude
import Data.Array (length, range, take, zipWith)
import Data.Foldable (class Foldable, foldl, foldMap, intercalate)
import Data.Newtype (ala, class Newtype, wrap)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Unfoldable (replicate)

-- | A text document.
newtype Doc = Doc
  { width  :: Int
  , height :: Int
  , lines  :: Array String
  }


instance docShow :: Show Doc where
    show (Doc doc) = intercalate "\n" doc.lines

-- | Get the width of a document.
width :: Doc -> Int
width (Doc doc) = doc.width

-- | Get the height of a document.
height :: Doc -> Int
height (Doc doc) = doc.height

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


foldExprCons :: Expr -> Doc
foldExprCons (ExprNil) = text ""
foldExprCons (ExprCons e es) = text ", " :<>: pretty e :<>: foldExprCons es
foldExprCons _ = text ""

foldExplCons :: Expl -> Doc
foldExplCons (ExplNil) = text ""
foldExplCons (ExplCons e es) = text ", " :<>: pretty e :<>: foldExplCons es
foldExplCons _ = text ""

foldValCons :: Val -> Doc
foldValCons (ValNil) = text ""
foldValCons (ValCons e es) = text ", " :<>: pretty e :<>: foldValCons es
foldValCons _ = text ""

instance exprPretty :: Pretty Expr where
    pretty (ExprInt n) = text (show n)
    pretty (ExprIntSel n) = text (show n)
    pretty (ExprVar x) = text x
    pretty ExprTrue    = text "true"
    pretty ExprTrueSel    = text "true"
    pretty ExprFalse   = text "false"
    pretty ExprFalseSel   = text "false"
    pretty (ExprPair e1 e2) = text "(" :<>: pretty e1 :<>: text ", " :<>: pretty e2 :<>: text ")"
    pretty (ExprPairSel e1 e2) = text  "(" :<>: pretty e1 :<>: text ", " :<>: pretty e2 :<>: text ")"
    pretty ExprNil = text "[]"
    pretty ExprNilSel = text "[]"
    pretty (ExprCons e es) = text "[" :<>: pretty e :<>: foldExprCons es :<>: text "]"
    pretty (ExprConsSel e es) = text "[" :<>: pretty e :<>: foldExprCons es :<>: text "]"
    pretty (ExprLet x e1 e2) = atop (text ("let " <>  x <> " = ") :<>: pretty e1)
                                    (text "in  " :<>: pretty e2)
    pretty (ExprMatch e elim) = atop (atop (text "match " :<>: pretty e :<>: text " as {") (pretty elim)) (text "}")
    pretty (ExprLetrec x elim e) = atop (text ("letrec " <>  x <> " = ") :<>: pretty elim)
                                        (text "in     " :<>: pretty e)
    pretty (ExprApp e1 e2) = pretty e1 :<>: text " " :<>: pretty e2
    pretty (ExprAdd e1 e2) = pretty e1 :<>: text " + " :<>: pretty e2


instance explPretty :: Pretty Expl where
    pretty ExplBottom  = text "⊥"
    pretty (ExplInt n) = text (show n)
    pretty (ExplVar x) = text x
    pretty ExplTrue    = text "true"
    pretty ExplFalse   = text "false"
    pretty (ExplPair e1 e2) = text "(" :<>: pretty e1 :<>: text ", " :<>: pretty e2 :<>: text ")"
    pretty ExplNil = text "[]"
    pretty (ExplCons e es) = text "[" :<>: pretty e :<>: foldExplCons es :<>: text "]"
    pretty (ExplLet x e1 e2) = atop (text ("let " <>  x <> " = ") :<>: pretty e1)
                                    (text "in  " :<>: pretty e2)
    pretty (ExplMatch e1 m e2) = atop (atop (text "match " :<>: pretty e1 :<>: text " as {") (pretty m)) (text "result = " :<>: pretty e2)
    pretty (ExplLetrec x elim e) = atop (text ("letrec " <>  x <> " = ") :<>: pretty elim)
                                        (text "in     " :<>: pretty e)
    pretty (ExplApp e1 e2 m e3) =  atop (atop (text "App (" :<>: pretty e1 :<>: text ", " :<>: pretty e2 :<>: text ")")
                                                   (text "     Match:  " :<>: pretty m))
                                                   (text "     Result: " :<>: pretty e3)
    pretty (ExplAdd e1 e2) = pretty e1 :<>: text " + " :<>: pretty e2
    pretty (ExplFun env elim) = text "Fun(" :<>:  (text "env \n") :<>: (pretty elim) :<>: text ")"


instance prettyMatch :: Pretty Match where
  pretty (MatchVar v )     = text "MatchVar " :<>: text v
  pretty (MatchPair t1 t2) = text "MatchPair " :<>: text t1 :<>: text " " :<>: text t2
  pretty (MatchNil)        = text "MatchNil "
  pretty (MatchCons x xs)  = text "MatchCons " :<>: text x :<>: text " " :<>: text xs
  pretty (MatchTrue)       = text "MatchTrue"
  pretty (MatchFalse)      = text "MatchFalse"


instance exprElim :: Pretty Elim where
    pretty (ElimVar { x, tx, e }) = text "  " :<>: text x :<>: text " : " :<>: pretty tx :<>: text " -> " :<>: pretty e
    pretty (ElimPair { x, tx, y, ty, e }) = text "   (" :<>: text x :<>: text " : " :<>: pretty tx :<>:
                                        text ", " :<>: text y :<>: text " : " :<>: pretty ty :<>: text ") -> " :<>: pretty e
    pretty (ElimList { bnil: e, bcons: { x, y, e: e' } }) = text "    " :<>: atop (text "[] -> " :<>: pretty e) (text "(" :<>: text x :<>: text ":" :<>: text y :<>: text ") -> " :<>: pretty e')
    pretty (ElimBool { btrue: e, bfalse: e' }) = text "     " :<>: atop (text "true -> " :<>: pretty e) (text "false -> " :<>: pretty e')

instance typPretty :: Pretty Typ where
    pretty TypBottom       = text "⊥"
    pretty TypInt          = text "Int"
    pretty TypBool         = text "Bool"
    pretty (TypFun a b)    = pretty a :<>: text " -> " :<>: pretty b
    pretty (TypList a)     = text "List " :<>: pretty a
    pretty (TypPair a b)   = text "Pair " :<>: pretty a :<>: text " " :<>: pretty b
    pretty (TypFailure s)  = text "Fail " :<>: text s

instance valPretty :: Pretty Val where
    pretty ValBottom = text "⊥"
    pretty (ValInt n)  = text $ show n
    pretty (ValIntSel n)  = text $ show n
    pretty ValTrue = text "True"
    pretty ValTrueSel = text "True"
    pretty ValFalse = text "False"
    pretty ValFalseSel = text "False"
    pretty (ValClosure env f elim) = text "Closure(" :<>: atop (text "env" :<>: text f ) (pretty elim) :<>: text ")"
    pretty (ValPair a b) = text "(" :<>: pretty a :<>: text ", " :<>: pretty b :<>: text ")"
    pretty (ValPairSel a b) = text "(" :<>: pretty a :<>: text ", " :<>: pretty b :<>: text ")"
    pretty ValNil = text "[]"
    pretty ValNilSel = text "[]"
    pretty (ValCons x xs) = text "[" :<>: pretty x :<>: foldValCons xs :<>: text "]"
    pretty (ValConsSel x xs) = text "[" :<>: pretty x :<>: foldValCons xs :<>: text "]"
    pretty (ValFailure s) = text s



instance explvalElim :: Pretty ExplVal where
    pretty (ExplVal {t, v}) = atop (pretty t) (pretty v)