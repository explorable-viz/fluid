module App.View.StackedBar2 where

import Prelude

import App.Util (Dimensions, Selectable, classes, contents)
import App.Util.Selector (dictVal)
import App.View.Segment2 (Segment, Scales)
import App.View.Util (Select, createRootElement2, setSelStates2)
import App.View.Util.D3 (ElementType(..), create, selectAll)
import App.View.Util.D3 as D3
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import DataType (f_segments)
import Effect (Effect)
import Util ((!))

newtype StackedBar = StackedBar
   { x :: Selectable String
   , segments :: Array Segment
   }

type StackedBarContext =
   { interior :: Dimensions Int
   , scales :: Scales
   , strokeWidth :: Int
   , y :: Number
   , y_index :: Int
   }

createRootElement' :: StackedBarContext -> StackedBar -> D3.Selection -> Effect D3.Selection
createRootElement' { interior, scales, strokeWidth, y, y_index } (StackedBar { x, segments }) parent = do
   g <- parent # create G [ classes [ "stack" ] ]
   for_ segments \segment ->
      createRootElement2 { interior, scales, strokeWidth, y, y_index, x: contents x } segment g
   pure g

setSelStates' :: StackedBarContext -> StackedBar -> Select -> D3.Selection -> Effect Unit
setSelStates' { interior, scales, strokeWidth, y, y_index } (StackedBar { x, segments }) select stackedBar = do
   segments' <- stackedBar # selectAll ".bar" -- TODO: .bar -> .segment
   forWithIndex_ segments' \j segment ->
      setSelStates2 { interior, scales, strokeWidth, y, y_index, x: contents x }
         (segments ! j)
         (select <<< dictVal f_segments)
         segment
