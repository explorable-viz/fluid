module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), Selectable, 𝕊(..), classes, colorShade, getPersistent, getTransient, selectionEventData')
import App.Util.Selector (ViewSelSetter, barChart, barSegment)
import App.View.LineChart (LegendEntry)
import App.View.Util (class Drawable, UIHelpers, Select, uiHelpers)
import App.View.Util.D3 (Coord, ElementType(..), Margin, colorScale, create, dimensions, remove, scaleBand, scaleLinear, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (range) as A
import Data.Array (length, mapWithIndex, uncons)
import Data.Foldable (for_, sum)
import Data.Int (floor, pow, toNumber, trunc)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number (ceil, log)
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Util (Endo, absurd, error, nonEmpty, (!))
import Web.Event.EventTarget (EventListener, eventListener)

newtype BarChart = BarChart
   { caption :: Selectable String
   , size :: Dimensions (Selectable Int)
   , stackedBars :: (Array StackedBar)
   }

newtype StackedBar = StackedBar
   { x :: Selectable String -- True × "Consumer"
   , bars :: (Array Bar)
   }

newtype Bar = Bar
   { y :: Selectable String
   , z :: Selectable Number
   }

type BarChartHelpers =
   { bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   , tickEvery :: Int -> Int
   , withBarChartSegment :: Select -> Effect EventListener
   }

nameCol :: String -> Array String -> String
nameCol = colorScale "schemeAccent"

foreign import createRootElement2 :: BarChartHelpers -> UIHelpers -> BarChart -> D3.Selection -> Effect D3.Selection
foreign import setSelStates2 :: BarChartHelpers -> BarChart -> Select -> D3.Selection -> Effect Unit

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' barchart@(BarChart { caption, size, stackedBars }) parent = do
   rootElement <- createRootElement2 barChartHelpers uiHelpers barchart parent
   { x: _xAxisHeight, y: _yAxisHeight } <- axisWidth rootElement
   let
      interior :: Dimensions Int
      interior = Dimensions
         { width: width - margin.left - margin.right
         , height: height - margin.top - margin.bottom - caption_height
         }
   createLegend interior rootElement
   for_ js \j -> do
      addHatchPattern rootElement j (indexCol j js)
   void $ createAxes interior rootElement
   rootElement
      # create Text
           [ "x" ⟼ width / 2
           , "y" ⟼ height - caption_height / 2
           , classes [ caption_class ]
           , "dominant-baseline" ↦ "central"
           , "text-anchor" ↦ "middle"
           ]
      >>= setText (fst caption)
   where
   names = case (uncons stackedBars) of
      Nothing -> error absurd
      Just { head: StackedBar bar, tail: _ } -> (\(Bar bar') -> fst bar'.y) <$> bar.bars
   xs = (\(StackedBar bar) -> fst $ bar.x) <$> stackedBars
   js = (A.range 0 (maximum (map (\(StackedBar bar) -> length bar.bars - 1) (nonEmpty stackedBars))))

   margin :: Margin
   margin =
      { top: 3
      , right: 75
      , bottom: 20
      , left: 30
      }
   Dimensions { width, height } = size <#> fst
   legendLineHeight = 15
   caption_class = "title-text"
   legendSquareSize = 4
   caption_height = textHeight caption_class (fst caption) * 2
   nearest = 10.0 :: Number
   y_max = ceil ((maximum $ (map (\(StackedBar bar) -> (sum $ map (\(Bar b) -> fst b.z) bar.bars)) (nonEmpty stackedBars))) / nearest) * nearest
   _y_ticks = barChartHelpers.tickEvery (y_max # trunc)

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior) parent' = do
      let Dimensions { height, width } = legend_dims
      legend' <- parent' # create G
         [ translate { x: interior.width + 30, y: max 0 ((interior.height - height) / 2) } ]
      void $ legend' # create Rect
         [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      for_ entries \{ i, name } -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText name
            )
         g # create Rect
            [ "fill" ↦ nameCol name names
            , "width" ⟼ legendSquareSize
            , "height" ⟼ legendSquareSize
            , "x" ⟼ legendLineHeight / 2 - legendSquareSize / 2
            , "y" ⟼ legendLineHeight / 2 - legendSquareSize
            ]
      where
      entries :: Array LegendEntry
      entries = flip mapWithIndex names \i name -> { i, name }
      entry_y i = i * legendLineHeight + 2
   legend_entry_x = 15

   legend_dims :: Dimensions Int
   legend_dims = Dimensions
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: legendLineHeight * length names
      }
      where
      maxTextWidth = maximum $ (names <#> textWidth "legend-text" # nonEmpty)
      rightMargin = 4

   axisWidth :: D3.Selection -> Effect (Coord Int)
   axisWidth parent' = do
      { x: xAxis, y: yAxis } <- createAxes (size <#> fst) parent'
      x <- dimensions xAxis <#> unwrap >>> _.height
      y <- dimensions yAxis <#> unwrap >>> _.width
      remove xAxis
      remove yAxis
      pure { x, y }

   createAxes :: Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
   createAxes range parent' = do
      -- let { x: _xLabels, y: _yLabels } = { x: names, y:  }
      x <- xAxis (to range) (nonEmpty xs) =<<
         (parent' # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap range).height } ])
      y <- yAxis (to range) 3.0 =<<
         (parent' # create G [ classes [ "y-axis" ] ])
      pure { x, y }

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: (scaleBand width fst (map ((\(StackedBar r) -> { x: r.x })) stackedBars))
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   addHatchPattern :: D3.Selection -> Int -> String -> Effect Unit
   addHatchPattern parent' j col_j = do
      pattern <- parent' # create Pattern
         [ "id" ↦ "diagonalHatch-" <> show j
         , "patternUnits" ↦ "userSpaceOnUse"
         , "width" ⟼ 2
         , "height" ⟼ 2
         , "patternTransform" ↦ "rotate(45)"
         ]
      void $ pattern # create Rect
         [ "width" ⟼ 3.5, "height" ⟼ 3.5, "fill" ↦ col_j ]
      void $ pattern # create Path
         [ "x1" ⟼ 0
         , "y" ⟼ 0
         , "x2" ⟼ 0
         , "y2" ⟼ 3.5
         , "stroke" ↦ "rgb(255, 255, 255, 1)"
         , "stroke-width" ↦ "1"
         ]

barChartHelpers :: BarChartHelpers
barChartHelpers =
   { bar_attrs
   , tickEvery
   , withBarChartSegment
   }
   where
   bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   bar_attrs indexCol' (BarChart { stackedBars }) { i, j } =
      fromFoldable
         [ "fill" ↦ case persistent of
              None -> col
              Secondary -> "url(#diagonalHatch-" <> show j <> ")"
              Primary -> colorShade col (-40)
         , "stroke-width" ↦ "1"
         , "stroke-dasharray" ↦ case transient of
              None -> "none"
              Secondary -> "0.5 1" -- "1 2"
              Primary -> "0.5 1" -- "2 2"
         , "stroke-linecap" ↦ "round"
         , "stroke" ↦
              if persistent /= None || transient /= None then colorShade col (-70)
              else col
         ]
      where
      StackedBar { bars } = stackedBars ! i
      Bar { z } = bars ! j
      t = snd z
      persistent = getPersistent t
      transient = getTransient t
      col = indexCol' j

   tickEvery :: Int -> Int
   tickEvery n =
      if n <= 2 * pow 10 m then 2 * pow 10 (m - 1)
      else pow 10 m
      where
      m = floor (log (toNumber n) / log 10.0)

   barChartSegment :: ViewSelSetter BarSegmentCoordinate
   barChartSegment { i, j } = barSegment i j >>> barChart

   withBarChartSegment :: Select -> Effect EventListener
   withBarChartSegment sel = eventListener $ sel <<< uncurry barChartSegment <<< selectionEventData'

indexCol :: Int -> Array Int -> String
indexCol = colorScale "schemeAccent"

instance Drawable BarChart where
   createRootElement = createRootElement'
   setSelStates = setSelStates2 barChartHelpers

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }

derive instance Newtype StackedBar _
