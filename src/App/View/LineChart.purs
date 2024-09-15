module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Dimensions(..), SelState, Selectable, 𝕊, colorShade, from, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import App.View.Util.Axes (Orientation(..))
import App.View.Util.D3 (Coord, Margin, SVGElementType(..), create, createMany, dimensions, each, forEach_create, isEmpty, line, nameCol, on, remove, rootSelect, rotate, scaleLinear, select, selectAll, setAttrs, setAttrs', setStyles, setText, setText_, textHeight, textWidth, translate, translate', xAxis, yAxis)
import App.View.Util.D3 (Selection) as D3
import App.View.Util.Point (Point(..))
import Bind (Bind, (↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, nub)
import Data.Foldable (length, sequence_)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_name, f_plots, f_points, f_size, f_tickLabels)
import Dict (Dict)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Primitive (string, unpack)
import Util (Endo, check, nonEmpty, (!))
import Util.Map (get)
import Val (BaseVal(..), Val(..))
import Web.Event.Event (EventType(..))

newtype LineChart = LineChart
   { size :: Dimensions (Selectable Int)
   , tickLabels :: Point Orientation
   , caption :: Selectable String
   , plots :: Array LinePlot
   }

newtype LinePlot = LinePlot
   { name :: Selectable String
   , points :: Array (Point Number)
   }

type LineChartHelpers =
   { createRootElement :: D3.Selection -> String -> Effect { rootElement :: D3.Selection, interior :: Dimensions Int }
   , point_attrs :: PointCoordinate -> Object String
   , point_attrs' :: PointCoordinate -> Array (Bind String)
   }

type LegendEntry =
   { i :: Int
   , name :: String
   }

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { size, tickLabels, plots, caption }) =
   { createRootElement
   , point_attrs
   , point_attrs'
   }
   where
   names :: Array String
   names = plots <#> unwrap >>> _.name >>> fst

   axisWidth :: D3.Selection -> Effect (Coord Int)
   axisWidth parent = do
      { x: xAxis, y: yAxis } <- createAxes (size <#> fst) parent
      x <- dimensions xAxis <#> unwrap >>> _.height
      y <- dimensions yAxis <#> unwrap >>> _.width
      remove xAxis
      remove yAxis
      pure { x, y }

   createRootElement :: D3.Selection -> String -> Effect { rootElement :: D3.Selection, interior :: Dimensions Int }
   createRootElement div childId = do
      svg <- create SVG div [ "width" ⟼ width, "height" ⟼ height, "id" ↦ childId ]
      { x: xAxisHeight, y: yAxisWidth } <- axisWidth svg

      let
         margin :: Margin
         margin =
            { top: point_smallRadius * 3 -- otherwise points at very top are clipped
            , right: 3 -- otherwise rightmost edge of legend box is clipped
            , bottom: xAxisHeight
            , left: yAxisWidth
            }

         interior :: Dimensions Int
         interior = Dimensions
            { width: width - margin.left - margin.right - (unwrap legend_dims).width - legend_sep
            , height: height - margin.top - margin.bottom - caption_height
            }

      g <- create G svg [ translate { x: margin.left, y: margin.top } ]
      void $ createAxes interior g
      createLines interior g
      createPoints interior g
      void $ setText (fst caption) =<< create Text svg
         [ "x" ⟼ width / 2
         , "y" ⟼ height - caption_height / 2
         , "class" ↦ caption_class
         , "dominant-baseline" ↦ "middle"
         , "text-anchor" ↦ "middle"
         ]
      createLegend interior g
      pure { rootElement: g, interior }
      where
      caption_class = "title-text"
      caption_height = textHeight caption_class (fst caption) * 2
      Dimensions { height, width } = size <#> fst

   createLines :: Dimensions Int -> D3.Selection -> Effect Unit
   createLines range parent =
      void $ createMany Path parent "linechart-line" entries
         [ "fill" ↦ const "none"
         , "stroke" ↦ \{ plot: LinePlot { name } } -> nameCol (fst name) names
         , "stroke-width" ↦ const "1"
         , "d" ↦ \{ plot: LinePlot { points: ps } } ->
              line (to range) (ps <#> \(Point { x, y }) -> { x: fst x, y: fst y })
         ]
      where
      entries :: Array { i :: Int, plot :: LinePlot }
      entries = flip mapWithIndex plots \i plot -> { i, plot }

   createPoints :: Dimensions Int -> D3.Selection -> Effect Unit
   createPoints range parent =
      void $ createMany Circle parent "linechart-point" entries
         [ "stroke-width" ↦ const "1"
         -- silly
         , "cx" ↦ \{ i, j } -> let Point { x } = (unwrap (plots ! i)).points ! j in show $ (to range).x (fst x)
         , "cy" ↦ \{ i, j } -> let Point { y } = (unwrap (plots ! i)).points ! j in show $ (to range).y (fst y)
         ]
      where
      entries :: Array PointCoordinate
      entries = concat $ flip mapWithIndex plots \i (LinePlot { name, points: ps }) ->
         flip mapWithIndex ps \j _ -> { name: fst name, i, j }

   point_attrs :: PointCoordinate -> Object String
   point_attrs = point_attrs' >>> fromFoldable

   point_attrs' :: PointCoordinate -> Array (Bind String)
   point_attrs' { i, j, name } =
      [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
      , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
      , "fill" ↦ fill col
      ]
      where
      Point { x, y } = (unwrap (plots ! i)).points ! j
      sel = snd x ∨ snd y
      col = nameCol name names
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

   legend_sep :: Int
   legend_sep = 15

   legend_dims :: Dimensions Int
   legend_dims = Dimensions
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: lineHeight * length plots
      }
      where
      maxTextWidth :: Int
      maxTextWidth = maximum (plots <#> unwrap >>> _.name >>> fst >>> textWidth "legend-text" # nonEmpty)

      rightMargin :: Int
      rightMargin = 4

   max' :: Coord Number
   max' = { x: maximum points.x, y: maximum points.y }

   min' :: Coord Number
   min' = { x: minimum points.x, y: minimum points.y }

   points :: Coord (NonEmptyArray Number)
   points = { x: ps <#> unwrap >>> _.x >>> fst, y: ps <#> unwrap >>> _.y >>> fst }
      where
      ps :: NonEmptyArray (Point Number)
      ps = plots <#> unwrap >>> _.points # join >>> nonEmpty

   to :: Dimensions Int -> Coord (Endo Number)
   to (Dimensions range) =
      { x: scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
      , y: scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
      }

   legend_entry_x :: Int
   legend_entry_x = 15

   createAxes :: Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
   createAxes range parent = do
      let Point { x: xLabels, y: yLabels } = tickLabels
      x <- xAxis (to range) (nub points.x) =<<
         create G parent [ "class" ↦ "x-axis", translate { x: 0, y: (unwrap range).height } ]
      when (fst xLabels == Rotated) do
         void $ selectAll x "text"
            >>= each (setAttrs [ rotate 45 ])
            >>= each (setStyles [ "text-anchor" ↦ "start" ])

      y <- yAxis (to range) 3.0 =<< create G parent [ "class" ↦ "y-axis" ]
      when (fst yLabels == Rotated) do
         void $ selectAll y "text"
            >>= each (setAttrs [ rotate 45 ])
            >>= each (setStyles [ "text-anchor" ↦ "end" ])
      pure { x, y }

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior) parent = do
      legend' <- create G parent
         [ translate { x: interior.width + legend_sep, y: max 0 ((interior.height - height) / 2) } ]
      void $ create Rect legend'
         [ "class" ↦ "legend-box", "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      legendEntries <- createMany G legend' "legend-entry" entries
         [ translate' \{ i } -> { x: 0, y: entry_y i } ]
      void $ each (setText_ (\{ name } -> name)) =<< forEach_create Text legendEntries
         [ "class" ↦ const "legend-text"
         , translate' $ const { x: legend_entry_x, y: 9 } -- align text with boxes
         ]
      void $ forEach_create Circle legendEntries
         [ "fill" ↦ \{ name } -> nameCol name names
         , "r" ↦ const (show point_smallRadius)
         , "cx" ↦ const (show circle_centre)
         , "cy" ↦ const (show circle_centre)
         ]

      where
      Dimensions { height, width } = legend_dims

      entries :: Array LegendEntry
      entries = flip mapWithIndex plots (\i (LinePlot { name }) -> { i, name: fst name })

      entry_y :: Int -> Int
      entry_y i = i * lineHeight + 2 -- tweak to emulate vertical centering of text

      circle_centre :: Int
      circle_centre = lineHeight / 2 - point_smallRadius / 2

   lineHeight :: Int
   lineHeight = 15

drawLineChart :: Renderer LineChart
drawLineChart _ { divId, suffix, view } redraw = do
   let { createRootElement, point_attrs' } = lineChartHelpers view
   let childId = divId <> "-" <> suffix
   div <- rootSelect ("#" <> divId)
   isEmpty div <#> not >>= flip check ("Unable to insert figure: no div found with id " <> divId)
   maybeRootElement <- select div ("#" <> childId)
   rootElement <- isEmpty maybeRootElement >>=
      if _ then createRootElement div childId <#> _.rootElement
      else pure maybeRootElement
   points <- selectAll rootElement ".linechart-point"
   void $ each (setAttrs' point_attrs') points
   sequence_ $ [ "mousedown", "mouseenter", "mouseleave" ]
      <#> \ev -> each (on (EventType ev) redraw) points

-- ======================
-- boilerplate
-- ======================

instance Drawable LineChart where
   draw rSpec figVal _ redraw =
      drawLineChart uiHelpers rSpec =<< selListener figVal redraw point
      where
      point :: ViewSelSetter PointCoordinate
      point { i, j } =
         linePoint j >>> listElement i >>> field f_plots >>> lineChart

instance Reflect (Dict (Val (SelState 𝕊))) LinePlot where
   from r = LinePlot
      { name: unpack string (get f_name r)
      , points: record from <$> from (get f_points r)
      }

instance Reflect (Dict (Val (SelState 𝕊))) LineChart where
   from r = LineChart
      { size: record from (get f_size r)
      , tickLabels: record from (get f_tickLabels r)
      , caption: unpack string (get f_caption r)
      , plots: from <$> (from (get f_plots r) :: Array (Val (SelState 𝕊))) :: Array LinePlot
      }

instance Reflect (Val (SelState 𝕊)) LinePlot where
   from (Val _ (Constr c (u : Nil))) | c == cLinePlot = record from u

derive instance Newtype LinePlot _
