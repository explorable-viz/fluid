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

--helperInt :: Maybe Int -> Int
--helperInt (Just x) = x
--helperInt (Nothing) = 0

-- takes a document and extracts the last line changing the last line into a document 
-- lastLine :: Doc -> Doc
-- lastLine (Doc d) = Doc { width: S.length (helperUnwrapMaybe (last d.lines)), height: 1, lines: singleton (helperUnwrapMaybe (last d.lines)) }

-- finds the string which has the longest length
-- longestString :: Array Int -> Int
-- longestString x = helperInt (last (sort x))

-- helper function which takes array of strings and returns array of integers where each integer is the length of the corresponding string
-- helperStringToInt :: Array String -> Array Int
-- helperStringToInt x = map S.length x

--findingLongestWidth :: Array String -> Int
--findingLongestWidth x = longestString (helperStringToInt x)

-- takes a document with n lines and extracts the first n-1 lines 
--allButLast :: Doc -> Doc
--allButLast (Doc d) = Doc { width: findingLongestWidth (take (A.length d.lines - 1) d.lines), height: d.height - 1, lines: take (A.length d.lines - 1) d.lines }

-- takes a document with n lines and extracts the last n-1 lines 
--allButFirst :: Doc -> Doc
--allButFirst (Doc d) = Doc { width: findingLongestWidth (drop 1 d.lines), height: d.height - 1, lines: drop 1 d.lines }

-- takes a document with n lines and extracts the first line 
--firstLine :: Doc -> Doc
--firstLine (Doc d) = Doc { width: S.length (helperUnwrapMaybe (head d.lines)), height: 1, lines: singleton (helperUnwrapMaybe (head d.lines)) }

-- indenting 
-- create an array of empty strings "" which has same number of elements as allButFirst (Doc d2)
-- each element is an empty string with spaces equal to length of lastLine (Doc d1)
-- then do zipWidth of this array and allButFirst 

-- here take last line of d1 and e.g. if this is "let" you would return [" ", " ", " "]
-- [" A "," A ", " A "]
--emptyStringArray :: Doc -> Array String
--emptyStringArray (Doc d) = replicate (d.width) "_"

emptyStringArray' :: String -> Array String
emptyStringArray' d = replicate (S.length d) " "

-- here you would take [" ", " ", " "] and form "   "
emptyStringConcat :: Array String -> String
emptyStringConcat x = foldl (<>) "" x

-- here you would take "   " and all but first of d2 and say this has 2 lines you would form ["   ", "   "]
--emptyStringRightIndentation :: Doc -> String -> Array String
--emptyStringRightIndentation (Doc d) x = replicate (A.length d.lines) x

emptyStringRightIndentation' :: Array String -> String -> Array String
emptyStringRightIndentation' p x = replicate (A.length p) x

-- here you would take ["   ", "   "] and all but first of d2 and form the required result 
--formingCorrectArray :: Array String -> Doc -> Array String
--formingCorrectArray x (Doc d) = zipWith (<>) x (d.lines)

formingCorrectArray' :: Array String -> Array String -> Array String
formingCorrectArray' x y = zipWith (<>) x y

-- here use the correct array to convert into a document 
--formingNewDocument :: Array String -> Doc -> Doc
--formingNewDocument x (Doc d) = Doc { width: findingLongestWidth x, height: d.height, lines: x }

-- helper function 
--temp :: Doc -> Doc -> Doc
--temp (Doc d1) (Doc d2) = formingNewDocument (formingCorrectArray (emptyStringRightIndentation (allButFirst (Doc d2)) (emptyStringConcat (emptyStringArray (lastLine (Doc d1))))) (allButFirst (Doc d2))) (allButFirst (Doc d2))

-- beside alternative 
--beside2 :: Doc -> Doc -> Doc
--beside2 (Doc d1) (Doc d2) = allButLast (Doc d1) .-. (lastLine (Doc d1) :<>: firstLine (Doc d2)) .-. (temp (Doc d1) (Doc d2))

allButLast' :: Doc -> Array String
allButLast' (Doc d) = take (A.length d.lines - 1) d.lines

allButFirst' :: Doc -> Array String
allButFirst' (Doc d) = drop 1 d.lines

firstLine' :: Doc -> String
firstLine' (Doc d) = helperUnwrapMaybe (head d.lines)

lastLine' :: Doc -> String
lastLine' (Doc d) = helperUnwrapMaybe (last d.lines)

indentations :: Doc -> Doc -> Array String
indentations (Doc d1) (Doc d2) = formingCorrectArray' (emptyStringRightIndentation' (allButFirst' (Doc d2)) (emptyStringConcat (emptyStringArray' (lastLine' (Doc d1))))) (allButFirst' (Doc d2))

finalLines :: Doc -> Doc -> Array String
finalLines (Doc d1) (Doc d2) = allButLast' (Doc d1) <> (singleton (lastLine' (Doc d1) <> "" <> firstLine' (Doc d2))) <> indentations (Doc d1) (Doc d2)

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
