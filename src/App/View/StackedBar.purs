module App.View.StackedBar
   ( StackedBar(..)
   ) where

import Prelude

import App.Segment (Segment(..))
import App.Util (PartialAttrs, Selectable, classes, contents)
import App.Util.Selector (dictVal)
import App.View.Util (class Drawable, Select, createRootElement, setSelStates)
import App.View.Util.D3 (ElementType(..), create, selectAll)
import App.View.Util.D3 as D3
import Data.Array (scanl)
import Data.Array.NonEmpty as A
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (class Newtype)
import DataType (f_segments)
import Effect (Effect)
import Util (nonEmpty, (!))

newtype StackedBar = StackedBar
   { x :: Selectable String
   , segments :: Array Segment
   }

createRootElement'
   :: PartialAttrs { x :: String, y :: Number, height :: Number } { y :: String, z :: Number }
   -> StackedBar
   -> D3.Selection
   -> Effect D3.Selection
createRootElement' attrFun (StackedBar { x, segments }) parent = do
   stack <- parent # create G [ classes [ "stack" ] ]
   forWithIndex_ barData \j bar ->
      createRootElement (\segment attrs' _ -> attrFun bar attrs' segment) (segments ! j) stack
   pure stack

   where
   barData :: Array { x :: String, y :: Number, height :: Number }
   barData =
      [ first ] <> scanl go first tail
      where
      { head: Segment { z }, tail } = A.uncons (nonEmpty segments)
      first = { x: contents x, y: 0.0, height: contents z }
      go { height, y } (Segment { z }) =
         { x: contents x, y: y + height, height: contents z }

setSelStates' :: StackedBar -> Select -> D3.Selection -> Effect Unit
setSelStates' (StackedBar { segments }) select stackedBar = do
   segments' <- stackedBar # selectAll ".bar" -- TODO: .bar -> .segment
   forWithIndex_ segments' \j segment ->
      setSelStates (segments ! j) (select <<< dictVal f_segments) segment

derive instance Newtype StackedBar _

instance Drawable StackedBar { x :: String, y :: Number, height :: Number } { y :: String, z :: Number } where
   createRootElement = createRootElement'
   setSelStates = setSelStates'
