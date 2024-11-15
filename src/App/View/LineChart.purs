module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Attrs, Dimensions(..), SelState, Selectable, 𝕊, classes, colorShade, dict, from, isPersistent, isPrimary, isSecondary, isTransient)
import App.Util.Selector (ViewSelSetter, dictVal, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.Axes (Orientation(..))
import App.View.Util.D3 (Coord, ElementType(..), Margin, colorScale, create, datum, dimensions, line, remove, rotate, scaleLinear, selectAll, setAttrs, setDatum, setStyles, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 (Selection) as D3
import App.View.Util.Point (Point(..))
import Bind ((↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, nub)
import Data.Foldable (for_, length)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_name, f_plots, f_points, f_size, f_tickLabels)
import Dict (Dict)
import Effect (Effect)
import Lattice ((∨), (∧))
import Primitive (string, unpack)
import Util (type (×), Endo, init, nonEmpty, tail, zipWith, (!), (×))
import Util.Map (get)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

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

fill :: SelState 𝕊 -> String -> String
fill sel = if isPersistent sel then flip colorShade (-30) else identity

nameCol :: String -> Array String -> String
nameCol = colorScale "schemePastel1"

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int }
type SegmentCoordinates = { i :: Int, j1 :: Int, j2 :: Int }
type Segment = { name :: String, start :: Coord Number, end :: Coord Number }

setSelState :: LineChart -> EventListener -> D3.Selection -> Effect Unit
setSelState (LineChart { plots }) redraw rootElement = do
   points <- rootElement # selectAll ".linechart-point"
   for_ points \point -> do
      point' <- datum point
      point # setAttrs (pointAttrs point') >>= registerMouseListeners redraw
   segments <- rootElement # selectAll ".linechart-segment"
   for_ segments \segment -> do
      segment' <- datum segment
      segment # setAttrs (segmentAttrs segment')
   where
   pointAttrs :: PointCoordinate -> Attrs
   pointAttrs { i, j } =
      [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
      , "stroke" ↦ (fill' # if isTransient sel then flip colorShade (-30) else identity)
      , "fill" ↦ fill'
      ]
      where
      LinePlot { name, points } = plots ! i
      sel = selState (points ! j)
      fill' = fill sel (nameCol (fst name) (names plots))

   segmentAttrs :: SegmentCoordinates -> Attrs
   segmentAttrs { i, j1, j2 } =
      [ "stroke" ↦ (fill' # if isTransient sel then flip colorShade (-30) else identity)
      , "stroke-width" ⟼ if isTransient sel then 2 else if isPersistent sel then 2 else 1
      ]
      where
      LinePlot { name, points } = plots ! i
      sel = selState (points ! j1) ∧ selState (points ! j2)
      fill' = fill sel (nameCol (fst name) (names plots))

   selState :: Point Number -> SelState 𝕊
   selState (Point { x, y }) = snd x ∨ snd y

createRootElement :: LineChart -> D3.Selection -> String -> Effect D3.Selection
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
   createLines range parent =
      for_ (concat $ mapWithIndex segments plots)
         \({ start, end } × segmentCoords) ->
            parent #
               ( create Path [ classes [ "linechart-segment" ], "d" ↦ line (to range) [ start, end ] ]
                    >=> setDatum segmentCoords
               )
      where
      segments :: Int -> LinePlot -> Array (Segment × SegmentCoordinates)
      segments i (LinePlot { name, points: ps }) = case fromArray ps of
         Nothing -> []
         Just ps' -> zipWith
            (\(start × j1) (end × j2) -> { name: fst name, start, end } × { i, j1, j2 })
            (mapWithIndex (\j point -> coord point × j) (init ps'))
            (mapWithIndex (\j point -> coord point × (j + 1)) (tail ps'))

      coord :: Point Number -> Coord Number
      coord (Point { x, y }) = { x: fst x, y: fst y }

   createPoints :: Dimensions Int -> D3.Selection -> Effect Unit
   createPoints range parent =
      for_ entries \(Point { x, y } × { i, j }) ->
         parent #
            ( create Circle
                 [ classes [ "linechart-point" ]
                 , "stroke-width" ⟼ 1
                 , "cx" ⟼ (to range).x (fst x)
                 , "cy" ⟼ (to range).y (fst y)
                 ]
                 >=> setDatum { i, j }
            )
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
      let circle_centre = lineHeight / 2 - point_smallRadius / 2
      for_ entries \{ i, name } -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            -- align text with boxes
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText name
            )
         g # create Circle
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

instance Drawable2 LineChart where
   setSelState = setSelState
   createRootElement = createRootElement

instance Drawable LineChart where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw point
      where
      point :: ViewSelSetter PointCoordinate
      point { i, j } =
         linePoint j >>> listElement i >>> dictVal f_plots >>> lineChart

-- ======================
-- boilerplate
-- ======================
instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) LinePlot where
   from r = LinePlot
      { name: unpack string (snd (get f_name r))
      , points: dict from <$> from (snd (get f_points r))
      }

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) LineChart where
   from r = LineChart
      { size: dict from (snd (get f_size r))
      , tickLabels: dict from (snd (get f_tickLabels r))
      , caption: unpack string (snd (get f_caption r))
      , plots: from <$> (from (snd (get f_plots r)) :: Array (Val (SelState 𝕊))) :: Array LinePlot
      }

instance Reflect (Val (SelState 𝕊)) LinePlot where
   from (Val _ (Constr c (u : Nil))) | c == cLinePlot = dict from u

derive instance Newtype LinePlot _
