module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Attrs, Dimensions(..), SelState, Selectable, 𝕊, classes, colorShade, from, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.Axes (Orientation(..))
import App.View.Util.D3 (Coord, ElementType(..), Margin, create, createMany, createMany', datum, dimensions, each, forEach_create, line, nameCol, remove, rotate, scaleLinear, selectAll, setAttrs, setDatum, setStyles, setText, setText_, textHeight, textWidth, translate, translate', xAxis, yAxis)
import App.View.Util.D3 (Selection) as D3
import App.View.Util.Point (Point(..))
import Bind ((↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, nub)
import Data.Foldable (for_, length)
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
import Util (type (×), Endo, nonEmpty, zip, (!), (×))
import Util.Map (get)
import Val (BaseVal(..), Val(..))

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
type PointCoordinate = { i :: Int, j :: Int }

instance Drawable2 LineChart where
   setSelState (LineChart { plots }) redraw rootElement = do
      points <- rootElement # selectAll ".linechart-point"
      for_ points \point -> do
         point' <- datum point
         point # setAttrs (selAttrs point') >>= registerMouseListeners redraw
      where
      selAttrs :: PointCoordinate -> Attrs
      selAttrs { i, j } =
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         ]
         where
         LinePlot { name, points } = plots ! i
         Point { x, y } = points ! j
         sel = snd x ∨ snd y
         col = nameCol (fst name) (names plots)
         fill = if isPersistent sel then flip colorShade (-30) else identity

   createRootElement (LineChart { size, tickLabels, caption, plots }) div childId = do
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
      void $ svg
         # create Text
              [ "x" ⟼ width / 2
              , "y" ⟼ height - caption_height / 2
              , classes [ caption_class ]
              , "dominant-baseline" ↦ "middle"
              , "text-anchor" ↦ "middle"
              ]
         >>= setText (fst caption)
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
            (parent # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap range).height } ])
         when (fst xLabels == Rotated) do
            labels <- x # selectAll "text"
            for_ labels $
               setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "start" ]
         y <- yAxis (to range) 3.0 =<< (parent # create G [ classes [ "y-axis" ] ])
         when (fst yLabels == Rotated) do
            labels <- y # selectAll "text"
            for_ labels $
               setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "end" ]
         pure { x, y }

      createLines :: Dimensions Int -> D3.Selection -> Effect Unit
      createLines range =
         void <$> createMany' Path plots
            [ "fill" ↦ const "none"
            , "stroke" ↦ \(LinePlot { name }) -> nameCol (fst name) (names plots)
            , "stroke-width" ↦ const "1"
            , "d" ↦ \(LinePlot { points: ps }) ->
                 line (to range) (ps <#> \(Point { x, y }) -> { x: fst x, y: fst y })
            ]

      createPoints :: Dimensions Int -> D3.Selection -> Effect Unit
      createPoints range parent = do
         ps <- parent # createMany' Circle entries
            [ "class" ↦ const "linechart-point"
            , "stroke-width" ↦ const "1"
            , "cx" ↦ \(Point { x } × _) -> show $ (to range).x (fst x)
            , "cy" ↦ \(Point { y } × _) -> show $ (to range).y (fst y)
            ]
         for_ (zip ps entries) \(point × _ × (p :: PointCoordinate)) ->
            point # setDatum p
         where
         entries :: Array (Point Number × PointCoordinate)
         entries = concat $ flip mapWithIndex plots \i (LinePlot { points: ps }) ->
            flip mapWithIndex ps \j p -> p × { i, j }

      createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
      createLegend (Dimensions interior) parent = do
         let Dimensions { height, width } = legend_dims
         legend' <- parent # create G
            [ translate { x: interior.width + legend_sep, y: max 0 ((interior.height - height) / 2) } ]
         void $ legend' # create Rect
            [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
         -- Re-express in terms of for and create (to lose data binding)
         legendEntries <- legend' # createMany G "legend-entry" entries
            [ translate' \{ i } -> { x: 0, y: entry_y i } ]
         void $ each (setText_ (\{ name } -> name)) =<<
            ( legendEntries # forEach_create Text \_ ->
                 [ classes [ "legend-text" ]
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
      draw' uiHelpers rSpec =<< selListener figVal redraw point
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
