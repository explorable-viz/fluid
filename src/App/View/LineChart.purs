module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Dimensions(..), SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import App.View.Util.D3 (Coord, SVGElementType(..))
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, nub)
import Data.Foldable (length)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd)
import DataType (cDefault, cLinePlot, cRotated, f_caption, f_name, f_plots, f_points, f_size, f_tickLabels, f_x, f_y)
import Dict (Dict)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Primitive (ToFrom, string, typeError, unpack)
import Util (Endo, nonEmpty, (!))
import Util.Map (get)
import Val (BaseVal(..), Val(..))

data Orientation
   = Default
   | Rotated

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

newtype Point a = Point (Coord (Selectable a))

type LineChartHelpers =
   { createRootElement :: D3.Selection -> String -> Effect { rootElement :: D3.Selection, interior :: Dimensions Int }
   , point_attrs :: Dimensions Int -> PointCoordinate -> Object String
   , legendHelpers :: LegendHelpers
   , createLegend :: Dimensions Int -> D3.Selection -> Effect D3.MultiSelection
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

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { size, tickLabels, plots, caption }) =
   { createRootElement
   , point_attrs
   , legendHelpers
   , createLegend
   }
   where
   names :: Array String
   names = plots <#> unwrap >>> _.name >>> fst

   axisWidth :: D3.Selection -> Effect (D3.Coord Int)
   axisWidth parent = do
      { x: xAxis, y: yAxis } <- createAxes (size <#> fst) parent
      x <- D3.dimensions xAxis <#> unwrap >>> _.height
      y <- D3.dimensions yAxis <#> unwrap >>> _.width
      D3.remove xAxis
      D3.remove yAxis
      pure { x, y }

   createRootElement :: D3.Selection -> String -> Effect { rootElement :: D3.Selection, interior :: Dimensions Int }
   createRootElement div childId = do
      svg <- D3.create SVG div [ "width" ⟼ width, "height" ⟼ height, "id" ↦ childId ]
      { x: xAxisHeight, y: yAxisWidth } <- axisWidth svg
      let
         margin :: D3.Margin
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

      g <- D3.create G svg [ D3.translate { x: margin.left, y: margin.top } ]
      void $ createAxes interior g
      createLines interior g
      createPoints g
      D3.setText (fst caption) =<< D3.create Text svg
         [ "x" ⟼ width / 2
         , "y" ⟼ height - caption_height / 2
         , "class" ↦ caption_class
         , "dominant-baseline" ↦ "middle"
         , "text-anchor" ↦ "middle"
         ]
      pure { rootElement: g, interior }
      where
      caption_class = "title-text"
      caption_height = D3.textHeight caption_class (fst caption) * 2
      Dimensions { height, width } = size <#> fst

   createLines :: Dimensions Int -> D3.Selection -> Effect Unit
   createLines range parent =
      void $ D3.createMany Path parent "linechart-line" entries
         [ "fill" ↦ const "none"
         , "stroke" ↦ \{ plot: LinePlot { name } } -> D3.nameCol (fst name) names
         , "stroke-width" ↦ const "1"
         , "d" ↦ \{ plot: LinePlot { points: ps } } ->
              D3.line (to range) (ps <#> \(Point { x, y }) -> { x: fst x, y: fst y })
         ]
      where
      entries :: Array { i :: Int, plot :: LinePlot }
      entries = flip mapWithIndex plots \i plot -> { i, plot }

   createPoints :: D3.Selection -> Effect Unit
   createPoints parent =
      void $ D3.createMany Circle parent "linechart-point" entries []
      where
      entries :: Array PointCoordinate
      entries = concat $ flip mapWithIndex plots \i (LinePlot { name, points: ps }) ->
         flip mapWithIndex ps \j _ -> { name: fst name, i, j }

   point_attrs :: Dimensions Int -> PointCoordinate -> Object String
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
      Point { x, y } = plot.points ! j
      sel = snd x ∨ snd y
      col = D3.nameCol name names
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
      maxTextWidth = maximum (plots <#> unwrap >>> _.name >>> fst >>> D3.textWidth legendEntry_class # nonEmpty)

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
      { x: D3.scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
      , y: D3.scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
      }

   legend_entry_x :: Int
   legend_entry_x = 15

   legendHelpers :: LegendHelpers
   legendHelpers =
      { text_attrs: fromFoldable
           [ D3.translate { x: legend_entry_x, y: 9 } -- align text with boxes
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

   createAxes :: Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
   createAxes range parent = do
      let Point { x: xLabels, y: yLabels } = tickLabels
      x <- D3.xAxis (to range) (nub points.x) =<<
         D3.create G parent [ "class" ↦ "x-axis", D3.translate { x: 0, y: (unwrap range).height } ]
      when (fst xLabels == Rotated) do
         labels <- D3.selectAll x "text"
         D3.setAttrs labels [ D3.rotate 45 ]
         D3.setStyles labels [ "text-anchor" ↦ "start" ]

      y <- D3.yAxis (to range) 3.0 =<< D3.create G parent [ "class" ↦ "y-axis" ]
      when (fst yLabels == Rotated) do
         labels <- D3.selectAll y "text"
         D3.setAttrs labels [ D3.rotate 45 ]
         D3.setStyles labels [ "text-anchor" ↦ "end" ]
      pure { x, y }

   createLegend :: Dimensions Int -> D3.Selection -> Effect D3.MultiSelection
   createLegend (Dimensions interior) parent = do
      legend' <- D3.create G parent
         [ D3.translate { x: interior.width + legend_sep, y: max 0 ((interior.height - height) / 2) } ]
      void $ D3.create Rect legend'
         [ "class" ↦ "legend-box", "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      legendEntries <- D3.createMany G legend' legendEntry_class entries
         [ D3.translate' \{ i } -> { x: 0, y: legendHelpers.entry_y i } ]
      D3.forEach_setText (\{ name } -> name) =<< D3.forEach_create Text legendEntries
         [ "class" ↦ const "legend-text"
         , D3.translate' $ const { x: legend_entry_x, y: 9 } -- align text with boxes
         ]
      pure legendEntries

      where
      Dimensions { height, width } = legend_dims

      entries :: Array LegendEntry
      entries = flip mapWithIndex plots (\i (LinePlot { name }) -> { i, name: fst name })

   legendEntry_class :: String
   legendEntry_class = "legend-entry"

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

derive instance Eq Orientation

-- Hefty amount of boilerplate just for a data type isomorphic to Bool
orientation :: forall a. ToFrom Orientation a
orientation =
   { pack: case _ of
        Default -> Constr cDefault Nil
        Rotated -> Constr cRotated Nil
   , unpack: case _ of
        Constr c Nil
           | c == cDefault -> Default
           | c == cRotated -> Rotated
        v -> typeError v "Orientation"
   }

instance Reflect (Dict (Val (SelState 𝕊))) (Point Number) where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val (SelState 𝕊))) (Point Orientation) where
   from r = Point
      { x: unpack orientation (get f_x r)
      , y: unpack orientation (get f_y r)
      }

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

derive instance Newtype (Point a) _
derive instance Newtype LinePlot _
