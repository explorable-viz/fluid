module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import App.View.Util.D3 (Coord, D3Selection, Dimensions, Margin, Ticks, createChild, createChildren, dimensions, line, nameCol, remove, scaleLinear, selectAll, text, textWidth, xAxis, yAxis)
import Bind ((↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (length)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
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

newtype Point = Point (Coord (Selectable Number))

type LineChartHelpers =
   { createRootElement :: D3Selection -> String -> Effect { rootElement :: D3Selection, interior :: Dimensions }
   , point_attrs :: Dimensions -> PointCoordinate -> Object String
   , legendHelpers :: LegendHelpers
   , createLegend :: Dimensions -> D3Selection -> Effect D3Selection
   , createLegendEntry :: D3Selection -> Effect D3Selection
   }

type LegendHelpers =
   { text_attrs :: Object String
   , circle_attrs :: Object String
   , entry_y :: Int -> Int
   }

type LegendEntry =
   { i :: Int
   , name :: String
   }

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

translate :: Coord Int -> String
translate { x, y } = "translate(" <> show x <> ", " <> show y <> ")"

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { plots, caption }) =
   { createRootElement
   , point_attrs
   , legendHelpers
   , createLegend
   , createLegendEntry
   }
   where
   names :: Array String
   names = plots <#> unwrap >>> _.name >>> fst

   -- Assume tick dimensions are independent of "range" that axes map into
   tickLength :: D3Selection -> Effect (Coord Int)
   tickLength parent = do
      { x: xAxis, y: yAxis } <- createAxes image parent
      let xDims = dimensions (selectAll xAxis ".tick")
          yDims = dimensions (selectAll yAxis ".tick")
      remove xAxis
      remove yAxis
      pure { x: maximum (xDims # nonEmpty <#> _.height), y: maximum (yDims # nonEmpty <#> _.width) }

   createRootElement :: D3Selection -> String -> Effect { rootElement :: D3Selection, interior :: Dimensions }
   createRootElement div childId = do
      svg <- createChild div "svg" $ fromFoldable
         [ "width" ⟼ image.width
         , "height" ⟼ image.height
         , "id" ↦ childId
         ]
      tickLen <- tickLength svg
      log (show tickLen)
      let
         margin :: Margin
         margin = { top: 15, right: 15, bottom: 40, left: tickLen.y }

         interior :: Dimensions
         interior =
            { width: image.width - margin.left - margin.right - legend_dims.width
            , height: image.height - margin.top - margin.bottom -- minus caption_height?
            }

      g <- createChild svg "g" $ fromFoldable
         [ "transform" ↦ translate { x: margin.left, y: margin.top }
         ]
      text (fst caption) =<< createChild svg "text" (fromFoldable
         [ "x" ⟼ image.width / 2
         , "y" ⟼ image.height - margin.bottom / 2
         , "class" ↦ "title-text"
         , "dominant-baseline" ↦ "middle"
         , "text-anchor" ↦ "middle"
         ])
      void $ createAxes interior g
      createLines interior g
      createPoints g
      pure { rootElement: g, interior }

   createLines :: Dimensions -> D3Selection -> Effect Unit
   createLines range parent =
      void $ createChildren parent "path" "linechart-line" entries $ fromFoldable
         [ "fill" ↦ const "none"
         , "stroke" ↦ \{ plot: LinePlot { name } } -> nameCol (fst name) names
         , "stroke-width" ↦ const "1"
         , "d" ↦ \{ plot: LinePlot { points: ps } } ->
            line (to range) (ps <#> \(Point { x, y }) -> { x: fst x, y: fst y } )
         ]
      where
      entries :: Array { i :: Int, plot :: LinePlot }
      entries = flip mapWithIndex plots \i plot -> { i, plot }

   createPoints :: D3Selection -> Effect Unit
   createPoints parent =
      void $ createChildren parent "circle" "linechart-point" entries $ fromFoldable []
      where
      entries :: Array PointCoordinate
      entries = concat $ flip mapWithIndex plots \i (LinePlot { name, points: ps }) ->
         flip mapWithIndex ps \j _ -> { name: fst name, i, j }

   point_attrs :: Dimensions -> PointCoordinate -> Object String
   point_attrs range { i, j, name } =
      fromFoldable
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
         , "stroke-width" ⟼ 1
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         , "cx" ⟼ (to range).x (fst x)
         , "cy" ⟼ (to range).y (fst y)
         ]
      where
      LinePlot plot = plots ! i
      Point { x , y } = plot.points ! j
      sel = snd x ∨ snd y
      col = nameCol name names
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

   legend_sep :: Int
   legend_sep = 15

   image :: Dimensions
   image = { width: max 330 (textWidth (fst caption)), height: 285 }

   legend_dims :: Dimensions
   legend_dims =
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: lineHeight * length plots
      }
      where
      maxTextWidth :: Int
      maxTextWidth = maximum (plots <#> unwrap >>> _.name >>> fst >>> textWidth # nonEmpty)

      rightMargin :: Int
      rightMargin = 4

   max' :: Coord Number
   max' = { x: maximum points.x, y: maximum points.y }

   min' :: Coord Number
   min' = { x: minimum points.x, y: minimum points.y }

   points :: Coord (NonEmptyArray Number)
   points = { x: ps <#> unwrap >>> _.x >>> fst, y: ps <#> unwrap >>> _.y >>> fst }
      where
      ps :: NonEmptyArray Point
      ps = plots <#> unwrap >>> _.points # join >>> nonEmpty

   to :: Dimensions -> Coord (Endo Number)
   to range =
      { x: scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
      , y: scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
      }

   legend_entry_x :: Int
   legend_entry_x = 15

   legendHelpers :: LegendHelpers
   legendHelpers =
      { text_attrs: fromFoldable
         [ "transform" ↦ translate { x: legend_entry_x, y: 9 } -- align text with boxes
         ]
      , circle_attrs: fromFoldable
         [ "r" ⟼ point_smallRadius
         , "cx" ⟼ circle_centre
         , "cy" ⟼ circle_centre
         ]
      , entry_y
      }
      where
      entry_y :: Int -> Int
      entry_y i = i * lineHeight + 2 -- tweak to emulate vertical centering of text

      circle_centre :: Int
      circle_centre = lineHeight / 2 - point_smallRadius / 2

   createAxes :: Dimensions -> D3Selection -> Effect (Coord D3Selection)
   createAxes range parent = do
      x <- xAxis (to range) tickValues.x =<< createChild parent "g" (fromFoldable
         [ "class" ↦ "x-axis"
         , "transform" ↦ translate { x: 0, y: range.height }
         ]
      )
      y <- yAxis (to range) ticks =<< createChild parent "g" (fromFoldable
         [ "class" ↦ "y-axis"
         ]
      )
      pure { x, y }
      where
      ticks :: Coord Ticks
      ticks = { x: max'.x - min'.x, y: 3.0 }

      tickValues :: Coord (NonEmptyArray Number)
      tickValues = { x: points.x, y: points.y }

   createLegend :: Dimensions -> D3Selection -> Effect D3Selection
   createLegend interior parent = do
      legend' <- createChild parent "g" $ fromFoldable
         [ "transform" ↦ translate
            { x: interior.width + legend_sep
            , y: max 0 ((interior.height - legend_dims.height) / 2)
            }
         ]
      void $ createChild legend' "rect" $ fromFoldable
         [ "class" ↦ "legend-box"
         , "x" ⟼ 0
         , "y" ⟼ 0
         , "height" ⟼ legend_dims.height
         , "width" ⟼ legend_dims.width
         ]
      pure legend'

   createLegendEntry :: D3Selection -> Effect D3Selection
   createLegendEntry parent =
      createChildren parent "g" "legend-entry" entries $ fromFoldable
         [ "transform" ↦ \{ i } -> translate { x: 0, y: legendHelpers.entry_y i } ]
      where
      entries :: Array LegendEntry
      entries = flip mapWithIndex plots (\i (LinePlot { name }) -> { i, name: fst name })

   lineHeight :: Int
   lineHeight = 15

foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart

-- ======================
-- boilerplate
-- ======================

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
   from (Val _ (Constr c (u : Nil))) | c == cLinePlot = record from u

derive instance Newtype Point _
derive instance Newtype LinePlot _
