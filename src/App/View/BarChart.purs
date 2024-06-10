module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState(..), Selectable, ViewSelector, 𝕊(..), colorShade, from, get_intOrNumber, record)
import App.Util.Selector (barChart, barSegment)
import App.View.Util (Renderer)
import DataType (f_bars, f_caption, f_data, f_x, f_y, f_z)
import Dict (Dict)
import Primitive (string, unpack)
import Util (Endo, (×))
import Util.Map (get)
import Val (Val)

newtype BarChart = BarChart
   { caption :: Selectable String
   , data :: Array StackedBar
   }

newtype StackedBar = StackedBar
   { x :: Selectable String
   , bars :: Array Bar
   }

newtype Bar = Bar
   { y :: Selectable String
   , z :: Selectable Number
   }

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val (SelState 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
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

bar_fill :: SelState 𝕊 -> Endo String
bar_fill s col = case s of
   SelState { persistent: None } -> col
   _ -> colorShade col (-20)

bar_stroke :: SelState 𝕊 -> Endo String
bar_stroke (SelState { persistent, transient }) col =
   case persistent × transient of
      None × None -> col
      _ -> colorShade col (-70)
