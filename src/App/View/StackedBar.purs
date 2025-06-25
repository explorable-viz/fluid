module App.View.StackedBar
   ( StackedBar(..)
   ) where

import Prelude

import App.Segment (Segment(..))
import App.Util (PartialAttrs, Selectable, classes, contents)
import App.Util.Selector (dictVal)
import App.View.Util (class Drawable, Select, createRootElement, setSelStates)
import App.View.Util.D3 (ElementType(..), create, datum, selectAll, setDatum)
import App.View.Util.D3 as D3
import Data.Array (scanl)
import Data.Array.NonEmpty as A
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (class Newtype)
import DataType (f_segments)
import Effect (Effect)
import Util (nonEmpty, (!))

newtype StackedBar = StackedBar
   { x :: Selectable String -- True × "Consumer"
   , segments :: (Array Segment)
   , i :: Int
   }

createRootElementStack :: PartialAttrs { x :: String, y :: Number, height :: Number } { y :: String, z :: Number } -> StackedBar -> D3.Selection -> Effect D3.Selection
createRootElementStack attrFun stackedBar@(StackedBar { i, segments }) parent' = do
   stack <- parent' # create G [ classes [ "stack" ] ] >>= setDatum { i }
   let barSegments = barData stackedBar

   forWithIndex_ barSegments \j bar' -> do
      void $ createRootElement
         (\segment attrs' _ -> attrFun bar' attrs' segment)
         (segments ! j)
         stack

   pure stack
   where
   barData :: StackedBar -> Array { x :: String, y :: Number, height :: Number }
   barData (StackedBar { x }) =
      [ first ] <> scanl go first tail
      where
      { head: Segment { z }, tail } = A.uncons (nonEmpty segments)
      first = { x: contents x, y: 0.0, height: contents z }
      go { height, y } (Segment { z }) =
         { x: contents x, y: y + height, height: contents z }

setSelStatesStack :: StackedBar -> Select -> D3.Selection -> Effect Unit
setSelStatesStack (StackedBar { segments }) select parent' = do
   segments' <- parent' # selectAll ".bar"
   for_ segments' \segment -> do
      { j } <- datum segment
      setSelStates (segments ! j) (select <<< dictVal f_segments) segment

derive instance Newtype StackedBar _

instance Drawable StackedBar { x :: String, y :: Number, height :: Number } { y :: String, z :: Number } where
   createRootElement = createRootElementStack
   setSelStates = setSelStatesStack

-- see data binding in .js
type BarSegmentCoordinate = { j :: Int }

