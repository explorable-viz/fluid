module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Bind ((↦))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict)
import Foreign.Object (Object, fromFoldable)
import Primitive (string, unpack)
import Util (Endo, nonEmpty, (!))
import Util.Map (get)
import Val (BaseVal(..), Val(..))

newtype LineChart = LineChart
   { caption :: Selectable String
   , plots :: Array LinePlot
   }

newtype LinePlot = LinePlot
   { name :: Selectable String
   , points :: Array Point
   }

newtype Point = Point
   { x :: Selectable Number
   , y :: Selectable Number
   }

type LineChartHelpers =
   { point_smallRadius :: Int
   , point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   , legendLineHeight :: Int
   , legendStart :: Int
   , margin :: Margin
   , width :: Int
   , height :: Int
   , x_ticks :: Ticks
   , y_ticks :: Ticks
   , to_x :: Endo Number
   , to_y :: Endo Number
   }

-- d3.js ticks are actually (start, stop, count) but we only supply first argument
type Ticks = Number

type Margin =
   { top :: Int
   , right :: Int
   , bottom :: Int
   , left :: Int
   }

foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { plots }) =
   { point_smallRadius
   , point_attrs
   , legendLineHeight: 15
   , legendStart: width + margin.left / 2
   , margin
   , width
   , height
   , x_ticks
   , y_ticks
   , to_x
   , to_y
   }
   where
   -- TODO: LineChart argument no longer needed
   point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   point_attrs nameCol _ { i, j, name } =
      fromFoldable
         [ "r" ↦ show (toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0)
         , "stroke-width" ↦ "1"
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         , "cx" ↦ show (to_x (fst x))
         , "cy" ↦ show (to_y (fst y))
         ]
      where
      LinePlot plot = plots ! i
      Point { x , y } = plot.points ! j
      sel = snd y  -- oof: discard x
      col = nameCol name
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

   margin :: Margin
   margin = { top: 15, right: 65, bottom: 40, left: 30 }

   width :: Int
   width = 330 - margin.left - margin.right

   height :: Int
   height = 285 - margin.top - margin.bottom

   y_max :: Number
   y_max = maximum (plots <#> plot_max_y # nonEmpty)
      where
      plot_max_y :: LinePlot -> Number
      plot_max_y (LinePlot { points }) = maximum (points # nonEmpty <#> \(Point { y }) -> fst y)

   x_min :: Number
   x_min = minimum (plots <#> plot_min_x # nonEmpty)
      where
      plot_min_x :: LinePlot -> Number
      plot_min_x (LinePlot { points }) = minimum (points # nonEmpty <#> \(Point { x }) -> fst x)

   x_max :: Number
   x_max = maximum (plots <#> plot_max_x # nonEmpty)
      where
      plot_max_x :: LinePlot -> Number
      plot_max_x (LinePlot { points }) = maximum (points # nonEmpty <#> \(Point { x }) -> fst x)

   to_x :: Number -> Number
   to_x = scaleLinear { min: x_min, max: x_max } { min: 0.0, max: toNumber width }

   to_y :: Number -> Number
   to_y = scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }

   x_ticks :: Ticks
   x_ticks = x_max - x_min

   y_ticks :: Ticks
   y_ticks = 3.0

foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart

instance Drawable LineChart where
   draw rSpec@{ view } figVal _ redraw =
      drawLineChart (lineChartHelpers view) uiHelpers rSpec =<< selListener figVal redraw point
      where
      point :: ViewSelSetter PointCoordinate
      point { i, j } =
         linePoint j >>> listElement i >>> field f_plots >>> lineChart

instance Reflect (Dict (Val (SelState 𝕊))) Point where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val (SelState 𝕊))) LinePlot where
   from r = LinePlot
      { name: unpack string (get f_name r)
      , points: record from <$> from ((get f_data r))
      }

instance Reflect (Dict (Val (SelState 𝕊))) LineChart where
   from r = LineChart
      { caption: unpack string (get f_caption r)
      , plots: from <$> (from (get f_plots r) :: Array (Val (SelState 𝕊))) :: Array LinePlot
      }

instance Reflect (Val (SelState 𝕊)) LinePlot where
   from (Val _ (Constr c (u1 : Nil))) | c == cLinePlot = record from u1

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }
