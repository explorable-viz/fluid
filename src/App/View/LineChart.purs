module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Attrs, Dimensions(..), SelState, Selectable, 𝕊, classes, colorShade, from, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.Axes (class HasAxes, axisWidth, createAxes, to)
import App.View.Util.D3 (Coord, ElementType(..), Margin, captionHeight, colorScale, create, createCaption, createSVG, datum, line, selectAll, setAttrs, setDatum, setText, textWidth, translate)
import App.View.Util.D3 (Selection) as D3
import App.View.Util.Orientation (Orientation)
import App.View.Util.Point (Point(..))
import Bind ((↦), (⟼))
import Data.Array (concat, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Foldable (for_, length)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst, snd)
import DataType (cLinePlot, f_caption, f_name, f_plots, f_points, f_size, f_tickLabels)
import Dict (Dict)
import Effect (Effect)
import Lattice ((∨), (∧))
import Primitive (string, unpack)
import Util (type (×), init, nonEmpty, tail, zipWith, (!), (×))
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
      , "stroke" ↦
           if isTransient sel then if isPrimary sel then "blue" else colorShade fill' (-70)
           else fill' -- not very general, but will do for now
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

instance HasAxes LineChart where
   points (LineChart { plots }) =
      { x: ps <#> unwrap >>> _.x >>> fst, y: ps <#> unwrap >>> _.y >>> fst }
      where
      ps :: NonEmptyArray (Point Number)
      ps = plots <#> unwrap >>> _.points # join >>> nonEmpty

   tickLabels = unwrap >>> _.tickLabels

createRootElement :: LineChart -> D3.Selection -> String -> Effect D3.Selection
createRootElement view@(LineChart { size, caption, plots }) div childId = do
   svg <- div # createSVG size' childId
   { x: xAxisHeight, y: yAxisWidth } <- axisWidth view size' svg

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
         , height: height - margin.top - margin.bottom - captionHeight (fst caption)
         }

   g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
   void $ createAxes view interior g
   createLines interior g
   createPoints interior g
   void $ svg # createCaption (unwrap size') (fst caption)
   createLegend interior g
   pure g
   where
   size'@(Dimensions { height, width }) = size <#> fst
   to' = to view

   createLines :: Dimensions Int -> D3.Selection -> Effect Unit
   createLines range parent =
      for_ (concat $ mapWithIndex segments plots)
         \({ start, end } × segmentCoords) ->
            parent #
               ( create Path [ classes [ "linechart-segment" ], "d" ↦ line (to' range) [ start, end ] ]
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
                 , "cx" ⟼ (to' range).x (fst x)
                 , "cy" ⟼ (to' range).y (fst y)
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
      for_ legend_entries \{ i, name } -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            -- align text with boxes
            (create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ] >=> setText name)
         g # create Circle
            [ "fill" ↦ nameCol name (names plots)
            , "r" ⟼ point_smallRadius
            , "cx" ⟼ circle_centre
            , "cy" ⟼ circle_centre
            ]
      where
      entry_y :: Int -> Int
      entry_y i = i * lineHeight + 2 -- tweak to emulate vertical centering of text

   legend_sep :: Int
   legend_sep = 15

   lineHeight :: Int
   lineHeight = 15

   legend_entries :: Array LegendEntry
   legend_entries = flip mapWithIndex plots (\i (LinePlot { name }) -> { i, name: fst name })

   legend_entry_x :: Int
   legend_entry_x = 15

   legend_dims :: Dimensions Int
   legend_dims = Dimensions
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: lineHeight * length plots
      }
      where
      maxTextWidth :: Int
      maxTextWidth = maximum (legend_entries <#> _.name >>> textWidth "legend-text" # nonEmpty)

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

derive instance Newtype LineChart _
derive instance Newtype LinePlot _
