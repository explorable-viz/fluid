module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, ViewSelector, 𝕊(..), colorShade, from, get_intOrNumber, isNone, isTransient, record)
import App.Util.Selector (barChart, barSegment)
import App.View.Util (Renderer)
import Bind ((↦))
import Data.Int (floor, pow, toNumber)
import Data.Newtype (unwrap)
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

foreign import drawBarChart :: BarChartHelpers -> Renderer BarChart

drawBarChart' :: Renderer BarChart
drawBarChart' = drawBarChart
   { bar_attrs
   , tickEvery
   }

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

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }

barChartSelector :: ViewSelector BarSegmentCoordinate
barChartSelector { i, j } = barSegment i j >>> barChart

bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
bar_attrs indexCol (BarChart { stackedBars }) { i, j } =
   fromFoldable
      [ "fill" ↦ fill
      , "stroke-dasharray" ↦ if isTransient sel then "1 1" else "none"
      , "stroke" ↦ (col # if not (isNone sel) then flip colorShade (-70) else identity)
      , "mask" ↦ if (unwrap sel).persistent == Secondary then "url(#diagonalHatch-mask)" else "none"
         -- hide element using this mask
      ]
   where
   StackedBar { bars } = stackedBars ! i
   Bar { z } = bars ! j
   sel = snd z
   col = indexCol j
   fill = col # if (unwrap sel).persistent == Primary then flip colorShade (-40) else identity

tickEvery :: Int -> Int
tickEvery n =
   if n <= 2 * pow 10 m then 2 * pow 10 (m - 1)
   else pow 10 m
   where
   m = floor (log (toNumber n) / log 10.0)
