module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelState, Selectable, 𝕊, colorShade, from, get_intOrNumber, isPersistent, isPrimary, isSecondary, isTransient, record)
import App.Util.Selector (ViewSelSetter, field, lineChart, linePoint, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Bind ((↦), (⟼))
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

newtype Point = Point
   { x :: Selectable Number
   , y :: Selectable Number
   }

type LineChartHelpers =
   { point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   , margin :: Margin
   , image_width :: Int
   , image_height :: Int
   , height :: Int
   , x_ticks :: Ticks
   , y_ticks :: Ticks
   , to_x :: Endo Number
   , to_y :: Endo Number
   , legendHelpers :: LegendHelpers
   , createLegend :: D3Selection -> Effect D3Selection
   , caption_attrs :: Object String
   }

type LegendHelpers =
   { text_attrs :: Object String
   , circle_attrs :: Object String
   , entry_y :: Int -> Int
   }

-- d3.js ticks are actually (start, stop, count) but we only supply first argument
type Ticks = Number

type Margin =
   { top :: Int
   , right :: Int
   , bottom :: Int
   , left :: Int
   }

translate :: Int -> Int -> String
translate x y = "translate(" <> show x <> ", " <> show y <> ")"

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number

lineChartHelpers :: LineChart -> LineChartHelpers
lineChartHelpers (LineChart { plots }) =
   { point_attrs
   , margin
   , image_height
   , image_width
   , height
   , x_ticks
   , y_ticks
   , to_x
   , to_y
   , legendHelpers
   , createLegend
   , caption_attrs
   }
   where
   -- TODO: LineChart argument no longer needed
   point_attrs :: (String -> String) -> LineChart -> PointCoordinate -> Object String
   point_attrs nameCol _ { i, j, name } =
      fromFoldable
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 2.0 else if isSecondary sel then 1.4 else 1.0
         , "stroke-width" ⟼ 1
         , "stroke" ↦ (fill col # if isTransient sel then flip colorShade (-30) else identity)
         , "fill" ↦ fill col
         , "cx" ⟼ to_x (fst x)
         , "cy" ⟼ to_y (fst y)
         ]
      where
      LinePlot plot = plots ! i
      Point { x , y } = plot.points ! j
      sel = snd y  -- oof: discard x
      col = nameCol name
      fill = if isPersistent sel then flip colorShade (-30) else identity

   point_smallRadius :: Int
   point_smallRadius = 2

   legend_sep :: Int
   legend_sep = 15

   margin :: Margin
   margin = { top: 15, right: 15, bottom: 40, left: 15 }

   image_width :: Int
   image_width = 330

   image_height :: Int
   image_height = 285

   width :: Int
   width = image_width - margin.left - margin.right - legend_width

   height :: Int
   height = image_height - margin.top - margin.bottom -- minus caption_height?

   legend_height :: Int
   legend_height = lineHeight * length plots

   legend_width :: Int
   legend_width = 40 -- could compute width based on text labels

   y_max :: Number
   y_max = maximum (plots <#> unwrap >>> _.points >>> ys >>> maximum # nonEmpty)

   x_min :: Number
   x_min = minimum (plots <#> unwrap >>> _.points >>> xs >>> minimum # nonEmpty)

   x_max :: Number
   x_max = maximum (plots <#> unwrap >>> _.points >>> xs >>> maximum # nonEmpty)

   xs :: Array Point -> NonEmptyArray Number
   xs = (_ # nonEmpty) >>> (_ <#> unwrap >>> _.x >>> fst)

   ys :: Array Point -> NonEmptyArray Number
   ys = (_ # nonEmpty) >>> (_ <#> unwrap >>> _.y >>> fst)

   to_x :: Number -> Number
   to_x = scaleLinear { min: x_min, max: x_max } { min: 0.0, max: toNumber width }

   to_y :: Number -> Number
   to_y = scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }

   x_ticks :: Ticks
   x_ticks = x_max - x_min

   y_ticks :: Ticks
   y_ticks = 3.0

   legend_x :: Int
   legend_x = width + legend_sep

   legend_y :: Int
   legend_y = (height - legend_height) / 2

   legendHelpers :: LegendHelpers
   legendHelpers =
      { text_attrs: fromFoldable
         [ "font-size" ⟼ 11
         , "transform" ↦ translate 15 9 -- align text with boxes
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
      legend <- createChild parent "g" $ fromFoldable
         [ "transform" ↦ translate legend_x legend_y ]
      void $ createChild legend "rect" $ fromFoldable
         [ "class" ↦ "legend-box"
         , "x" ⟼ 0
         , "y" ⟼ 0
         , "height" ⟼ legend_height
         , "width" ⟼ legend_width
         ]
      pure legend

   lineHeight :: Int
   lineHeight = 15

   caption_attrs :: Object String
   caption_attrs = fromFoldable
      [ "x" ⟼ width / 2
      , "y" ⟼ height + 35
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
   from (Val _ (Constr c (u1 : Nil))) | c == cLinePlot = record from u1

-- 0-based indices of line plot and point within line plot; see data binding in .js
type PointCoordinate = { i :: Int, j :: Int, name :: String }

derive instance Newtype Point _
derive instance Newtype LinePlot _
