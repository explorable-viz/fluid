module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Dimensions(..), SelState, Selectable, 𝕊, colorShade, from, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', selListener, uiHelpers)
import App.View.Util.Axes (Orientation(..))
import App.View.Util.D3 (Coord, Margin, ElementType(..), create, createMany, dimensions, each, forEach_create, line, nameCol, on, remove, rotate, scaleLinear, selectAll, setAttrs, setAttrs', setStyles, setText, setText_, textHeight, textWidth, translate, translate', xAxis, yAxis)
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
import Lattice ((∨))
import Primitive (string, unpack)
import Util (Endo, nonEmpty, (!))
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

type LegendEntry =
   { i :: Int
   , name :: String
   }

names :: Array LinePlot -> Array String
names plots = plots <#> unwrap >>> _.name >>> fst

point_smallRadius :: Int
point_smallRadius = 2

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

instance Drawable2 LineChart Unit where
   setSelState (LineChart { plots }) _ redraw rootElement = do
      points' <- selectAll rootElement ".linechart-point"
      void $ each (setAttrs' selAttrs) points'
      sequence_ $ [ "mousedown", "mouseenter", "mouseleave" ]
         <#> \ev -> each (on (EventType ev) redraw) points'
      where
      selAttrs :: PointCoordinate -> Array (Bind String)
      selAttrs { i, j, name } =
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         ]
         where
         Point { x, y } = (unwrap (plots ! i)).points ! j
         sel = snd x ∨ snd y
         col = nameCol name (names plots)
         fill = if isPersistent sel then flip colorShade (-30) else identity

   createRootElement (LineChart { size, tickLabels, caption, plots }) _ div childId = do
      svg <- div # create SVG [ "width" ⟼ width, "height" ⟼ height, "id" ↦ childId ]
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

      g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
      void $ createAxes interior g
      createLines interior g
      createPoints interior g
      void $ setText (fst caption) =<<
         ( svg # create Text
              [ "x" ⟼ width / 2
              , "y" ⟼ height - caption_height / 2
              , "class" ↦ caption_class
              , "dominant-baseline" ↦ "middle"
              , "text-anchor" ↦ "middle"
              ]
         )
      createLegend interior g
      pure g

      where
      caption_class = "title-text"
      caption_height = textHeight caption_class (fst caption) * 2
      Dimensions { height, width } = size <#> fst

      axisWidth :: D3.Selection -> Effect (Coord Int)
      axisWidth parent = do
         { x: xAxis, y: yAxis } <- createAxes (size <#> fst) parent
         x <- dimensions xAxis <#> unwrap >>> _.height
         y <- dimensions yAxis <#> unwrap >>> _.width
         remove xAxis
         remove yAxis
         pure { x, y }

      createAxes :: Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
      createAxes range parent = do
         let Point { x: xLabels, y: yLabels } = tickLabels
         x <- xAxis (to range) (nub points.x) =<<
            (parent # create G [ "class" ↦ "x-axis", translate { x: 0, y: (unwrap range).height } ])
         when (fst xLabels == Rotated) do
            void $ selectAll x "text"
               >>= each (setAttrs [ rotate 45 ])
               >>= each (setStyles [ "text-anchor" ↦ "start" ])

         y <- yAxis (to range) 3.0 =<< (parent # create G [ "class" ↦ "y-axis" ])
         when (fst yLabels == Rotated) do
            void $ selectAll y "text"
               >>= each (setAttrs [ rotate 45 ])
               >>= each (setStyles [ "text-anchor" ↦ "end" ])
         pure { x, y }

      createLines :: Dimensions Int -> D3.Selection -> Effect Unit
      createLines range =
         void <$> createMany Path "linechart-line" entries
            [ "fill" ↦ const "none"
            , "stroke" ↦ \{ plot: LinePlot { name } } -> nameCol (fst name) (names plots)
            , "stroke-width" ↦ const "1"
            , "d" ↦ \{ plot: LinePlot { points: ps } } ->
                 line (to range) (ps <#> \(Point { x, y }) -> { x: fst x, y: fst y })
            ]
         where
         entries :: Array { i :: Int, plot :: LinePlot }
         entries = flip mapWithIndex plots \i plot -> { i, plot }

      createPoints :: Dimensions Int -> D3.Selection -> Effect Unit
      createPoints range =
         void <$> createMany Circle "linechart-point" entries
            [ "stroke-width" ↦ const "1"
            -- silly
            , "cx" ↦ \{ i, j } -> let Point { x } = (unwrap (plots ! i)).points ! j in show $ (to range).x (fst x)
            , "cy" ↦ \{ i, j } -> let Point { y } = (unwrap (plots ! i)).points ! j in show $ (to range).y (fst y)
            ]
         where
         entries :: Array PointCoordinate
         entries = concat $ flip mapWithIndex plots \i (LinePlot { name, points: ps }) ->
            flip mapWithIndex ps \j _ -> { name: fst name, i, j }

      createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
      createLegend (Dimensions interior) parent = do
         let Dimensions { height, width } = legend_dims
         legend' <- parent # create G
            [ translate { x: interior.width + legend_sep, y: max 0 ((interior.height - height) / 2) } ]
         void $ legend' # create Rect
            [ "class" ↦ "legend-box", "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
         legendEntries <- legend' # createMany G "legend-entry" entries
            [ translate' \{ i } -> { x: 0, y: entry_y i } ]
         void $ each (setText_ (\{ name } -> name)) =<<
            ( legendEntries # forEach_create Text \_ ->
                 [ "class" ↦ "legend-text"
                 , translate { x: legend_entry_x, y: 9 } -- align text with boxes
                 ]
            )
         let circle_centre = lineHeight / 2 - point_smallRadius / 2
         void $ legendEntries # forEach_create Circle \{ name } ->
            [ "fill" ↦ nameCol name (names plots)
            , "r" ⟼ point_smallRadius
            , "cx" ⟼ circle_centre
            , "cy" ⟼ circle_centre
            ]

         where
         entries :: Array LegendEntry
         entries = flip mapWithIndex plots (\i (LinePlot { name }) -> { i, name: fst name })

         entry_y :: Int -> Int
         entry_y i = i * lineHeight + 2 -- tweak to emulate vertical centering of text

      min' :: Coord Number
      min' = { x: minimum points.x, y: minimum points.y }

      max' :: Coord Number
      max' = { x: maximum points.x, y: maximum points.y }

      to :: Dimensions Int -> Coord (Endo Number)
      to (Dimensions range) =
         { x: scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
         , y: scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
         }

      points :: Coord (NonEmptyArray Number)
      points = { x: ps <#> unwrap >>> _.x >>> fst, y: ps <#> unwrap >>> _.y >>> fst }
         where
         ps :: NonEmptyArray (Point Number)
         ps = plots <#> unwrap >>> _.points # join >>> nonEmpty

      legend_sep :: Int
      legend_sep = 15

      lineHeight :: Int
      lineHeight = 15

      legend_entry_x :: Int
      legend_entry_x = 15

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

instance Drawable LineChart where
   draw rSpec figVal _ redraw =
      draw' unit uiHelpers rSpec =<< selListener figVal redraw point
      where
      point :: ViewSelSetter PointCoordinate
      point { i, j } =
         linePoint j >>> listElement i >>> field f_plots >>> lineChart

-- ======================
-- boilerplate
-- ======================

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
