module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Bind ((↦), (⟼))
import Data.Array (mapWithIndex)
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
   { createRootElement :: D3Selection -> String -> Effect D3Selection
   , point_attrs :: (String -> String) -> PointCoordinate -> Object String
   , interior :: Dimensions
   , ticks :: Coord Ticks
   , to :: Coord (Endo Number)
   , legendHelpers :: LegendHelpers
   , createLegend :: D3Selection -> Effect D3Selection
   , createLegendEntry :: D3Selection -> Effect D3Selection
   , caption_attrs :: Object String
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

-- d3.js ticks are actually (start, stop, count) but we only supply first argument
type Ticks = Number

type Margin =
   { top :: Int
   , right :: Int
   , bottom :: Int
   , left :: Int
   }

type Coord a =
   { x :: a
   , y :: a
   }

type Dimensions =
   { width :: Int
   , height :: Int
   }

translate :: Coord Int -> String
translate { x, y } = "translate(" <> show x <> ", " <> show y <> ")"

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import createChildren :: forall a. D3Selection -> String -> Array a -> Object (a -> String) -> Effect D3Selection
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { plots }) =
   { createRootElement
   , point_attrs
   , interior
   , ticks
   , to
   , legendHelpers
   , createLegend
   , createLegendEntry
   , caption_attrs
   }
   where
   createRootElement :: D3Selection -> String -> Effect D3Selection
   createRootElement div childId = do
      rootElement <- createChild div "svg" $ fromFoldable
         [ "width" ⟼ image.width
         , "height" ⟼ image.height
         , "id" ↦ childId
         ]
      createChild rootElement "g" $ fromFoldable
         [ "transform" ↦ translate { x: margin.left, y: margin.top }
         ]

   -- TODO: LineChart argument no longer needed
   point_attrs :: (String -> String) -> PointCoordinate -> Object String
   point_attrs nameCol { i, j, name } =
      fromFoldable
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
         , "stroke-width" ⟼ 1
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         , "cx" ⟼ to.x (fst x)
         , "cy" ⟼ to.y (fst y)
         ]
      where
      LinePlot plot = plots ! i
      Point { x , y } = plot.points ! j
      sel = snd x ∨ snd y
      col = nameCol name
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

   legend_sep :: Int
   legend_sep = 15

   margin :: Margin
   margin = { top: 15, right: 15, bottom: 40, left: 15 }

   image :: Dimensions
   image = { width: 330, height: 285 }

   interior :: Dimensions
   interior =
      { width: image.width - margin.left - margin.right - legend_dims.width
      , height: image.height - margin.top - margin.bottom -- minus caption_height?
      }

   legend_dims :: Dimensions
   legend_dims =
      { width: 40 -- could compute width based on text labels
      , height: lineHeight * length plots
      }

   max :: Coord Number
   max = { x: maximum points.x, y: maximum points.y }

   min :: Coord Number
   min = { x: minimum points.x, y: minimum points.y }

   points :: Coord (NonEmptyArray Number)
   points = { x: ps <#> unwrap >>> _.x >>> fst, y: ps <#> unwrap >>> _.y >>> fst }
      where
      ps :: NonEmptyArray Point
      ps = plots <#> unwrap >>> _.points # join >>> nonEmpty

   to :: Coord (Endo Number)
   to =
      { x: scaleLinear { min: min.x, max: max.x } { min: 0.0, max: toNumber interior.width }
      , y: scaleLinear { min: 0.0, max: max.y } { min: toNumber interior.height, max: 0.0 }
      }

   ticks :: Coord Ticks
   ticks = { x: max.x - min.x, y: 3.0 }

   legend :: Coord Int
   legend = { x: interior.width + legend_sep, y: (interior.height - legend_dims.height) / 2 }

   legendHelpers :: LegendHelpers
   legendHelpers =
      { text_attrs: fromFoldable
         [ "font-size" ⟼ 11
         , "transform" ↦ translate { x: 15, y: 9 } -- align text with boxes
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

   createLegend :: D3Selection -> Effect D3Selection
   createLegend parent = do
      legend' <- createChild parent "g" $ fromFoldable
         [ "transform" ↦ translate { x: legend.x, y: legend.y } ]
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
      createChildren parent "g" entries $ fromFoldable
         [ "transform" ↦ \{ i } -> translate { x: 0, y: legendHelpers.entry_y i } ]
      where
      entries :: Array LegendEntry
      entries = mapWithIndex ((\i (LinePlot { name }) -> { i, name: fst name })) plots

   lineHeight :: Int
   lineHeight = 15

   caption_attrs :: Object String
   caption_attrs = fromFoldable
      [ "x" ⟼ interior.width / 2
      , "y" ⟼ interior.height + 35
      , "class" ↦ "title-text"
      , "dominant-baseline" ↦ "bottom"
      , "text-anchor" ↦ "middle"
      ]

foreign import drawLineChart :: LineChartHelpers -> Renderer LineChart

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

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

derive instance Newtype Point _
derive instance Newtype LinePlot _
