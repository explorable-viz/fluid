module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, {-ReactState, Relectable,-} SelState, Selectable, ViewSelector, 𝕊, colorShade, from, {- fromℝ,-} get_intOrNumber, {- get_intOrNumberℝ,-} isPersistent, isPrimary, isSecondary, isTransient, record {-}, recordℝ-} )
import App.Util.Selector (field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, {-} RRenderer,-} Renderer, selListener, uiHelpers {-}, uiRHelpers-} )
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

{-}
newtype RLineChart = RLineChart
   { caption :: Relectable String
   , rplots :: Array RLinePlot
   }

newtype RLinePlot = RLinePlot
   { name :: Relectable String
   , rpoints :: Array RPoint
   }

newtype RPoint = RPoint
   { x :: Relectable Number
   , y :: Relectable Number
   }
-}
type LineChartHelpers =
   { plot_max_x :: LinePlot -> Number
   , plot_min_x :: LinePlot -> Number
   , plot_max_y :: LinePlot -> Number
   , point_smallRadius :: Int
   , point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   }

{-}
type RLineChartHelpers =
   { rplot_max_x :: RLinePlot -> Number
   , rplot_min_x :: RLinePlot -> Number
   , rplot_max_y :: RLinePlot -> Number
   , point_smallRadius :: Int
   , rpoint_attrs :: (String -> String) -> RLineChart -> PointCoordinate -> Object String
   }
-}
foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart Unit

--foreign import drawRLineChart :: RLineChartHelpers -> RRenderer RLineChart Unit

drawLineChart' :: Renderer LineChart Unit
drawLineChart' = drawLineChart
   { plot_max_x
   , plot_min_x
   , plot_max_y
   , point_smallRadius
   , point_attrs
   }

{-}
drawRLineChart' :: RRenderer RLineChart Unit
drawRLineChart' = drawRLineChart
   { rplot_max_x
   , rplot_min_x
   , rplot_max_y
   , point_smallRadius
   , rpoint_attrs
   }
-}
instance Drawable LineChart Unit where
   draw divId suffix redraw view viewState =
      drawLineChart' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw lineChartSelector
      where
      lineChartSelector :: ViewSelector PointCoordinate
      lineChartSelector { i, j } =
         lineChart <<< field f_plots <<< listElement i <<< linePoint j

{-}
instance Drawable RLineChart Unit where
   draw divId suffix redraw view viewState =
      drawRLineChart' { uiRHelpers, divId, suffix, view, viewState } =<< selListener redraw lineChartSelector
      where
      lineChartSelector :: ViewSelector PointCoordinate
      lineChartSelector { i, j } =
         lineChart <<< field f_plots <<< listElement i <<< linePoint j
-}
instance Reflect (Dict (Val (SelState 𝕊))) Point where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

{-}
instance Reflect (Dict (Val (ReactState 𝕊))) RPoint where
   from r = RPoint
      { x: get_intOrNumberℝ f_x r
      , y: get_intOrNumberℝ f_y r
      }
-}
instance Reflect (Dict (Val (SelState 𝕊))) LinePlot where
   from r = LinePlot
      { name: unpack string (get f_name r)
      , points: record from <$> from (get f_data r)
      }

{-
instance Reflect (Dict (Val (ReactState 𝕊))) RLinePlot where
   from r = RLinePlot
      { name: unpack string (get f_name r)
      , rpoints: recordℝ from <$> from (get f_data r)
      }
-}
-- not sure if this is used anywhere?
-- used in view
instance Reflect (Dict (Val (SelState 𝕊))) LineChart where
   from r = LineChart
      { caption: unpack string (get f_caption r)
      , plots: from <$> (from (get f_plots r) :: Array (Val (SelState 𝕊))) :: Array LinePlot
      }

{-
instance Reflect (Dict (Val (ReactState 𝕊))) RLineChart where
   from r = RLineChart
      { caption: unpack string (get f_caption r)
      , rplots: recordℝ from <$> (from (get f_plots r) :: Array (Val (ReactState 𝕊))) :: Array RLinePlot
      }
-}
instance Reflect (Val (SelState 𝕊)) LinePlot where
   from (Val _ (Constr c (u1 : Nil))) | c == cLinePlot = record from u1

{-
instance Reflect (Val (ReactState 𝕊)) RLinePlot where
   from (Val _ (Constr c (u1 : Nil))) | c == cLinePlot = recordℝ from u1
-}
-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

point_smallRadius :: Int
point_smallRadius = 2

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

{-
rpoint_attrs :: (String -> String) -> RLineChart -> PointCoordinate -> Object String
rpoint_attrs nameCol (RLineChart { rplots }) { i, j, name } =
   fromFoldable
      [ "r" ↦ show (toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0)
      , "stroke-width" ↦ "1"
      , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
      , "fill" ↦ fill col
      ]
   where
   RLinePlot rplot = rplots ! i
   RPoint { y } = rplot.rpoints ! j
   sel = fromℝ (snd y)
   col = nameCol name
   fill = if isPersistent sel then flip colorShade (-30) else identity
-}
plot_max_y :: LinePlot -> Number
plot_max_y (LinePlot { points }) = definitely' (maximum (points <#> \(Point { y }) -> fst y))

plot_min_x :: LinePlot -> Number
plot_min_x (LinePlot { points }) = definitely' (minimum (points <#> \(Point { x }) -> fst x))

plot_max_x :: LinePlot -> Number
plot_max_x (LinePlot { points }) = definitely' (maximum (points <#> \(Point { x }) -> fst x))

{-
rplot_max_x :: RLinePlot -> Number
rplot_max_x (RLinePlot { rpoints }) = definitely' (maximum (rpoints <#> \(RPoint { x }) -> fst x))

rplot_max_y :: RLinePlot -> Number
rplot_max_y (RLinePlot { rpoints }) = definitely' (maximum (rpoints <#> \(RPoint { y }) -> fst y))

rplot_min_x :: RLinePlot -> Number
rplot_min_x (RLinePlot { rpoints }) = definitely' (minimum (rpoints <#> \(RPoint { x }) -> fst x))
-}