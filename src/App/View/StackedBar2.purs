module App.View.StackedBar2 where

import Prelude

import App.Util (Dimensions, Selectable, classes, contents)
import App.Util.Selector (dictVal)
import App.View.Segment2 (Scales, Segment(..), SegmentContext)
import App.View.Util (class View2, Select, createRootElement2, setSelStates2)
import App.View.Util.D3 (ElementType(..), create, selectAll)
import App.View.Util.D3 as D3
import Data.Array (scanl)
import Data.Array.NonEmpty (cons', init)
import Data.Foldable (sum)
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
   }

barHeight :: StackedBar -> Number
barHeight (StackedBar bar) = sum (map (\(Segment seg) -> contents seg.z) bar.segments)

instance View2 StackedBar StackedBarContext where
   createRootElement2 :: StackedBarContext -> StackedBar -> D3.Selection -> Effect D3.Selection
   createRootElement2 context (stackedBar@(StackedBar { segments })) parent = do
      g <- parent # create G [ classes [ "stack" ] ]
      forWithIndex_ segments \y_index segment ->
         createRootElement2 (segmentContext context stackedBar y_index) segment g
      pure g

   setSelStates2 :: StackedBarContext -> StackedBar -> Select -> D3.Selection -> Effect Unit
   setSelStates2 context (stackedBar@(StackedBar { segments })) select root = do
      -- Might be more robust and more consistent with createRootElement to iterate over segments instead
      segments' <- root # selectAll ".bar" -- TODO: .bar -> .segment
      forWithIndex_ segments' \y_index segment ->
         setSelStates2 (segmentContext context stackedBar y_index)
            (segments ! y_index)
            (select <<< dictVal f_segments)
            segment

segmentContext :: StackedBarContext -> StackedBar -> Int -> SegmentContext
segmentContext { interior, scales, strokeWidth } (StackedBar { x, segments }) =
   \y_index ->
      { interior, scales, strokeWidth, x: contents x, y: ys ! y_index, y_index }
   where
   ys :: Array Number
   ys = init (cons' 0.0 (scanl (+) 0.0 (segments <#> \(Segment seg) -> contents seg.z)))
