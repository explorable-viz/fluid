module App.View.StackedBar
   ( StackedBar(..)
   ) where

import Prelude

import App.Segment (Segment(..))
import App.Util (PartialAttrs, Selectable, classes, contents)
import App.Util.Selector (dictVal, listElement)
import App.View.Util (class Drawable, Select, createRootElement, setSelStates)
import App.View.Util.D3 (ElementType(..), create, datum, selectAll, setDatum)
import App.View.Util.D3 as D3
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as A
import Data.Foldable (foldl, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (class Newtype)
import DataType (f_bars)
import Effect (Effect)
import Util (nonEmpty, (!))

newtype StackedBar = StackedBar
   { x :: Selectable String -- True × "Consumer"
   , bars :: (Array Segment)
   , i :: Int
   }

createRootElementStack :: PartialAttrs { x :: String, y :: Number, height :: Number } { y :: String, z :: Number } -> StackedBar -> D3.Selection -> Effect D3.Selection
createRootElementStack attrFun stackedBar@(StackedBar { i, bars }) parent' = do
   stack <- parent' # create G [ classes [ "stack" ] ] >>= setDatum { i }
   let barSegments = barData stackedBar

   forWithIndex_ barSegments \j bar' -> do
      void $ createRootElement
         (\segment attrs' _ -> attrFun bar' attrs' segment)
         (bars ! j)
         stack

   pure stack
   where
   barData :: StackedBar -> NonEmptyArray { x :: String, y :: Number, height :: Number }
   barData (StackedBar { x }) =
      foldl go first tail
      where
      { head: Segment { z }, tail } = A.uncons (nonEmpty bars)
      first = A.singleton { x: xv, y: 0.0, height: contents z }
      xv = contents x

      go acc (Segment { z }) =
         A.snoc acc { x: xv, y: y + height, height: contents z }
         where
         { y, height } = A.last acc

setSelStatesStack :: StackedBar -> Select -> D3.Selection -> Effect Unit
setSelStatesStack (StackedBar { bars, i }) select parent' = do
   segments <- parent' # selectAll ".bar"
   for_ segments \segment -> do
      { j } <- datum segment
      setSelStates (bars ! j) (select <<< listElement i <<< dictVal f_bars) segment

derive instance Newtype StackedBar _

instance Drawable StackedBar { x :: String, y :: Number, height :: Number } { y :: String, z :: Number } where
   createRootElement = createRootElementStack
   setSelStates = setSelStatesStack

-- see data binding in .js
type BarSegmentCoordinate = { j :: Int }

