module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.SelSetter (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Bind ((↦))
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict)
import Foreign.Object (Object, fromFoldable)
import Primitive (string, unpack)
import Util (definitely', (!))
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
   { plot_max_x :: LinePlot -> Number
   , plot_min_x :: LinePlot -> Number
   , plot_max_y :: LinePlot -> Number
   , point_smallRadius :: Int
   , point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   }

lineChartHelpers :: LineChartHelpers
lineChartHelpers =
   { plot_max_x
   , plot_min_x
   , plot_max_y
   , point_smallRadius
   , point_attrs
   }
   where
   plot_max_x :: LinePlot -> Number
   plot_max_x (LinePlot { points }) = definitely' (maximum (points <#> \(Point { x }) -> fst x))

   plot_min_x :: LinePlot -> Number
   plot_min_x (LinePlot { points }) = definitely' (minimum (points <#> \(Point { x }) -> fst x))

   plot_max_y :: LinePlot -> Number
   plot_max_y (LinePlot { points }) = definitely' (maximum (points <#> \(Point { y }) -> fst y))

   point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   point_attrs nameCol (LineChart { plots }) { i, j, name } =
      fromFoldable
         [ "r" ↦ show (toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0)
         , "stroke-width" ↦ "1"
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         ]
      where
      LinePlot plot = plots ! i
      Point { y } = plot.points ! j
      sel = snd y
      col = nameCol name
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart

instance Drawable LineChart where
   draw rSpec figVal _ redraw =
      drawLineChart lineChartHelpers uiHelpers rSpec =<< selListener figVal redraw point
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
      , points: record from <$> from (get f_data r)
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
