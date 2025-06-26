module App.View.StackedBar2 where

import Prelude

import App.Util (Dimensions, Selectable, classes, contents)
import App.View.Segment2 (Segment, Scales)
import App.View.Util (createRootElement2)
import App.View.Util.D3 (ElementType(..), create)
import App.View.Util.D3 as D3
import Data.Foldable (for_)
import Effect (Effect)

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
