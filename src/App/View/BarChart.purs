module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (fromℝ, class Reflect, ReactState, Relectable, SelState(..), ViewSelector, 𝕊(..), colorShade, from, get_intOrNumber, recordℝ)
import App.Util.Selector (barChart, barSegment)
import App.View.Util (class Drawable, RRenderer, selListener, uiRHelpers)
import Bind ((↦))
import Data.Int (floor, pow, toNumber)
import Data.Number (log)
import Data.Tuple (snd)
import DataType (f_bars, f_caption, f_data, f_x, f_y, f_z)
import Dict (Dict)
import Foreign.Object (Object, fromFoldable)
import Primitive (string, unpack)
import Util ((!))
import Util.Map (get)
import Val (Val)

newtype BarChart = BarChart
   { caption :: Relectable String
   , stackedBars :: Array StackedBar
   }

newtype StackedBar = StackedBar
   { x :: Relectable String
   , bars :: Array Bar
   }

newtype Bar = Bar
   { y :: Relectable String
   , z :: Relectable Number
   }

type BarChartHelpers =
   { bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   , tickEvery :: Int -> Int
   }

foreign import drawBarChart :: BarChartHelpers -> RRenderer BarChart Unit

drawBarChart' :: RRenderer BarChart Unit
drawBarChart' = drawBarChart
   { bar_attrs
   , tickEvery
   }

instance Drawable BarChart Unit where
   draw divId suffix redraw view viewState =
      drawBarChart' { uiRHelpers, divId, suffix, view, viewState } =<< selListener redraw barChartSelector
      where
      barChartSelector :: ViewSelector BarSegmentCoordinate
      barChartSelector { i, j } = barSegment i j >>> barChart

instance Reflect (Dict (Val (ReactState 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , stackedBars: recordℝ from <$> from (get f_data r)
      }

instance Reflect (Dict (Val (ReactState 𝕊))) StackedBar where
   from r = StackedBar
      { x: unpack string (get f_x r)
      , bars: recordℝ from <$> from (get f_bars r)
      }

instance Reflect (Dict (Val (ReactState 𝕊))) Bar where
   from r = Bar
      { y: unpack string (get f_y r)
      , z: get_intOrNumber f_z r
      }

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }

bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
bar_attrs indexCol (BarChart { stackedBars }) { i, j } =
   fromFoldable
      [ "fill" ↦ case persistent of
           None -> col
           Secondary -> "url(#diagonalHatch-" <> show j <> ")"
           Primary -> colorShade col (-40)
      , "stroke-width" ↦ "1.5"
      , "stroke-dasharray" ↦ case transient of
           None -> "none"
           Secondary -> "1 2"
           Primary -> "2 2"
      , "stroke-linecap" ↦ "round"
      , "stroke" ↦
           if persistent /= None || transient /= None then colorShade col (-70)
           else col
      ]
   where
   -- ok, so implement inert into images
   StackedBar { bars } = stackedBars ! i
   Bar { z } = bars ! j
   --if z is inert, then what on earth do we do here?
   SelState { persistent, transient } = fromℝ (snd z)
   col = indexCol j

tickEvery :: Int -> Int
tickEvery n =
   if n <= 2 * pow 10 m then 2 * pow 10 (m - 1)
   else pow 10 m
   where
   m = floor (log (toNumber n) / log 10.0)

{-}
newtype BarChart = BarChart
   { caption :: Selectable String
   , stackedBars :: Array StackedBar
   }

newtype StackedBar = StackedBar
   { x :: Selectable String
   , bars :: Array Bar
   }

newtype Bar = Bar
   { y :: Selectable String
   , z :: Selectable Number
   }

type BarChartHelpers =
   { bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   , tickEvery :: Int -> Int
   }

foreign import drawBarChart :: BarChartHelpers -> Renderer BarChart Unit

drawBarChart' :: Renderer BarChart Unit
drawBarChart' = drawBarChart
   { bar_attrs
   , tickEvery
   }


instance Drawable BarChart Unit where
   draw divId suffix redraw view viewState =
      drawBarChart' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw barChartSelector
      where
      barChartSelector :: ViewSelector BarSegmentCoordinate
      barChartSelector { i, j } = barSegment i j >>> barChart

instance Reflect (Dict (Val (SelState 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , stackedBars: record from <$> from (get f_data r)
      }

instance Reflect (Dict (Val (SelState 𝕊))) StackedBar where
   from r = StackedBar
      { x: unpack string (get f_x r)
      , bars: record from <$> from (get f_bars r)
      }

instance Reflect (Dict (Val (SelState 𝕊))) Bar where
   from r = Bar
      { y: unpack string (get f_y r)
      , z: get_intOrNumber f_z r
      }


bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
bar_attrs indexCol (BarChart { stackedBars }) { i, j } =
   fromFoldable
      [ "fill" ↦ case persistent of
           None -> col
           Secondary -> "url(#diagonalHatch-" <> show j <> ")"
           Primary -> colorShade col (-40)
      , "stroke-width" ↦ "1.5"
      , "stroke-dasharray" ↦ case transient of
           None -> "none"
           Secondary -> "1 2"
           Primary -> "2 2"
      , "stroke-linecap" ↦ "round"
      , "stroke" ↦
           if persistent /= None || transient /= None then colorShade col (-70)
           else col
      ]
   where
   StackedBar { bars } = stackedBars ! i
   Bar { z } = bars ! j
   SelState { persistent, transient } = snd z
   col = indexCol j
-}