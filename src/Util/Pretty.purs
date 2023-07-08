-- | This module defines a set of combinators for pretty printing text.

module Util.Pretty
   ( Doc
   , width
   , height
   , render
   , empty
   , text
   , atop
   , Stack(..)
   , vcat
   , Columns(..)
   , hcat
   , beside
   , space
   ) where

import Prelude
import Data.Array (range, take, zipWith, last, singleton, head, drop)
import Data.Array (length) as A
import Data.Foldable (class Foldable, foldl, foldMap, intercalate)
import Data.Newtype (ala, class Newtype, wrap)
import Data.String as S
-- import Data.String.CodeUnits as SCU
import Data.Unfoldable (replicate)
import Data.Maybe (Maybe(..))

-- | A text document.
newtype Doc = Doc
   { width :: Int
   , height :: Int
   , lines :: Array String
   }

derive instance Eq Doc

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
   Doc
      { width: w
      , height: h
      , lines: case h of
           0 -> []
           _ -> range 1 h $> ""
      }

-- | Create a document from some text.
text :: String -> Doc
text s =
   Doc
      { width: foldl max 0 $ map S.length lines
      , height: A.length lines
      , lines: lines
      }
   where
   lines = S.split (wrap "\n") s

-- | Place one document beside another.
-- beside :: Doc -> Doc -> Doc
-- beside (Doc d1) (Doc d2) =
--    Doc
--       { width: d1.width + d2.width
--       , height: height_
--       , lines: take height_ $ zipWith append (map (padRight d1.width) (adjust d1)) (adjust d2)
--       }
--    where
--    height_ :: Int
--    height_ = max d1.height d2.height

--    -- Adjust a document to fit the new width and height
--    adjust :: { lines :: Array String, width :: Int, height :: Int } -> Array String
--    adjust d = d.lines <> replicate (height_ - d.height) (emptyLine d.width)

--    emptyLine :: Int -> String
--    emptyLine w = SCU.fromCharArray (replicate w ' ' :: Array Char)

--    padRight :: Int -> String -> String
--    padRight w s = s <> emptyLine (w - S.length s)

-- | Place one document on top of another.
atop :: Doc -> Doc -> Doc
atop (Doc d1) (Doc d2) =
   Doc
      { width: max d1.width d2.width
      , height: d1.height + d2.height
      , lines: d1.lines <> d2.lines
      }

-- | Beside with a space
space :: Doc -> Doc -> Doc
space x y = x :<>: text " " :<>: y

infixl 5 beside as :<>:
infixl 5 atop as .-.
infixl 5 space as :--:

helperUnwrapMaybe :: Maybe String -> String
helperUnwrapMaybe (Just x) = x
helperUnwrapMaybe (Nothing) = ""

indentedExpressionRequired :: String -> Array String
indentedExpressionRequired d = replicate (S.length d) " "

-- here you would take [" ", " ", " "] and form "   "
spacesNeeded :: Array String -> String
spacesNeeded x = foldl (<>) "" x

-- here you would take "   " and all but first of d2 and say this has 2 lines you would form ["   ", "   "]
indentingEachLine :: Array String -> String -> Array String
indentingEachLine p x = replicate (A.length p) x

-- here you would take ["   ", "   "] and all but first of d2 and form the required result 
indentedExpressionHelper :: Array String -> Array String -> Array String
indentedExpressionHelper x y = zipWith (<>) x y

allButLast :: Doc -> Array String
allButLast (Doc d) = take (A.length d.lines - 1) d.lines

allButFirst :: Doc -> Array String
allButFirst (Doc d) = drop 1 d.lines

firstLine :: Doc -> String
firstLine (Doc d) = helperUnwrapMaybe (head d.lines)

lastLine :: Doc -> String
lastLine (Doc d) = helperUnwrapMaybe (last d.lines)

indentedExpression :: Doc -> Doc -> Array String
indentedExpression (Doc d1) (Doc d2) = indentedExpressionHelper (indentingEachLine (allButFirst (Doc d2)) (spacesNeeded (indentedExpressionRequired (lastLine (Doc d1))))) (allButFirst (Doc d2))

finalLines :: Doc -> Doc -> Array String
finalLines (Doc d1) (Doc d2) = allButLast (Doc d1) <> (singleton (lastLine (Doc d1) <> "" <> firstLine (Doc d2))) <> indentedExpression (Doc d1) (Doc d2)

beside :: Doc -> Doc -> Doc
beside (Doc d1) (Doc d2) = Doc { width: d1.width + d2.width, height: d1.height + d2.height, lines: finalLines (Doc d1) (Doc d2) }

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
