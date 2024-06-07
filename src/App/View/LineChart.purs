module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState(..), Selectable, ViewSelector, 𝕊(..), colorShade, from, get_intOrNumber, record)
import App.Util.Selector (field, lineChart, linePoint, listElement)
import App.View.Util (Renderer)
import Bind ((↦))
import Data.Foldable (maximum, minimum)
import Data.List (List(..), (:))
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict, fromFoldable)
import Foreign.Object (Object)
import Primitive (string, unpack)
import Util (definitely', (!), (×))
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

foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart

drawLineChart' :: Renderer LineChart
drawLineChart' = drawLineChart
   { plot_max_x
   , plot_min_x
   , plot_max_y
   , point_smallRadius
   , point_attrs
   }

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

lineChartSelector :: ViewSelector PointCoordinate
lineChartSelector { i, j } =
   lineChart <<< field f_plots <<< listElement i <<< linePoint j

point_smallRadius :: Int
point_smallRadius = 2

point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
point_attrs nameCol (LineChart { plots }) { i, j, name } =
   case persistent × transient of
      None × None -> fromFoldable
         [ "r" ↦ show point_smallRadius
         , "stroke" ↦ col
         ]
      _ -> fromFoldable
         [ "r" ↦ show (point_smallRadius * 2)
         , "stroke" ↦ colorShade col (-30)
         ]
   where
   LinePlot plot = plots ! i
   Point { y } = plot.points ! j
   SelState { persistent, transient } = snd y
   col = nameCol name

plot_max_y :: LinePlot -> Number
plot_max_y (LinePlot { points }) = definitely' (maximum (points <#> \(Point { y }) -> fst y))

plot_min_x :: LinePlot -> Number
plot_min_x (LinePlot { points }) = definitely' (minimum (points <#> \(Point { x }) -> fst x))

plot_max_x :: LinePlot -> Number
plot_max_x (LinePlot { points }) = definitely' (maximum (points <#> \(Point { x }) -> fst x))
