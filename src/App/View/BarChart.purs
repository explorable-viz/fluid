module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (Attrs, Dimensions(..), Selectable, 𝕊(..), PartialAttrs, classes, colorShade, getPersistent, getTransient, selectionEventData')
import App.Util.Selector (ViewSelSetter, barChart, barSegment)
import App.View.LineChart (LegendEntry)
import App.View.Util (class Drawable, Select, registerMouseListeners)
import App.View.Util.D3 (Coord, ElementType(..), Margin, bandwidth, colorScale, create, datum, scaleBand, scaleLinear, selectAll, setAttrs, setDatum, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (elemIndex, foldl, length, mapWithIndex, range)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, last, singleton, snoc, uncons) as A
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (ceil)
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object)
import Util (Endo, definitely', nonEmpty, (!))
import Web.Event.EventTarget (EventListener, eventListener)

newtype BarChart = BarChart
   { caption :: Selectable String
   , size :: Dimensions (Selectable Int)
   , stackedBars :: (Array StackedBar)
   }

newtype StackedBar = StackedBar
   { x :: Selectable String -- True × "Consumer"
   , bars :: (Array Bar)
   , i :: Int
   }

newtype Bar = Bar
   { y :: Selectable String
   , z :: Selectable Number
   , j :: Int
   }

type BarChartHelpers =
   { bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   , tickEvery :: Int -> Int
   , withBarChartSegment :: Select -> Effect EventListener
   }

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' (BarChart { caption, size, stackedBars }) parent = do
   svg <- parent # create SVG [ "width" ⟼ width, "height" ⟼ height ]

   g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
   void $ createAxes g
   createStacks g 1

   for_ js \j -> do
      addHatchPattern g j $ indexCol $ definitely' $ elemIndex j js

   void $ svg
      # create Text
           [ "x" ⟼ width / 2
           , "y" ⟼ height - caption_height / 2
           , classes [ caption_class ]
           , "dominant-baseline" ↦ "central"
           , "text-anchor" ↦ "middle"
           ]
      >>= setText (fst caption)

   createLegend interior g
   pure g
   where
   stackedBars' = nonEmpty stackedBars
   -- Assuming all bars have the same set of names
   ys = bar.bars <#> \(Bar bar') -> fst bar'.y
      where
      StackedBar bar = A.head stackedBars'

   xs = stackedBars <#> \(StackedBar bar) -> fst bar.x
   js = range 0 $ length ys - 1

   Dimensions { width, height } = size <#> fst

   margin :: Margin
   margin =
      { top: 3
      , right: 75
      , bottom: 20
      , left: 30
      }

   interior :: Dimensions Int
   interior = Dimensions
      { width: width - margin.left - margin.right
      , height: height - margin.top - margin.bottom - caption_height
      }

   caption_class = "title-text"
   caption_height = textHeight caption_class (fst caption) * 2

   legend_entry_x = 15
   legendSquareSize = 4
   legendLineHeight = 15

   legend_dims :: Dimensions Int
   legend_dims = Dimensions
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: legendLineHeight * length ys
      }
      where
      maxTextWidth = maximum (ys <#> textWidth "legend-text" # nonEmpty)
      rightMargin = 4

   scales = to interior
   nearest = 10.0
   y_max = ceil $ ((maximum $ map (\(StackedBar bar) -> (sum $ map (\(Bar b) -> fst b.z) bar.bars)) stackedBars') / nearest) * nearest

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: scaleBand width fst $ map (\(StackedBar r) -> { x: r.x }) stackedBars
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   createStacks :: D3.Selection -> Int -> Effect Unit
   createStacks parent' strokeWidth = do
      for_ stackedBars \stackedBar -> do
         createStack attrFun parent' stackedBar
      where
      attrFun bar =
         [ "x" ⟼ scales.x bar.x
         , "y" ⟼ scales.y (bar.y + bar.height)
         , "stroke-width" ⟼ strokeWidth
         , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth) - scales.y bar.height
         , "width" ⟼ bandwidth scales.x
         ]

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior') parent' = do
      let Dimensions { height, width } = legend_dims
      legend' <- parent' # create G
         [ translate { x: interior'.width + 30, y: max 0 $ (interior'.height - height) / 2 } ]
      void $ legend' # create Rect
         [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      for_ entries \{ i, name } -> do
         let nameIndex = definitely' $ elemIndex name ys
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText name
            )
         g # create Rect
            [ "fill" ↦ indexCol nameIndex
            , "width" ⟼ legendSquareSize
            , "height" ⟼ legendSquareSize
            , "x" ⟼ legendLineHeight / 2 - legendSquareSize / 2
            , "y" ⟼ legendLineHeight / 2 - legendSquareSize
            ]
      where
      entries :: Array LegendEntry
      entries = flip mapWithIndex ys \i name -> { i, name }
      entry_y i = i * legendLineHeight + 2

   createAxes :: D3.Selection -> Effect (Coord D3.Selection)
   createAxes parent' = do
      x <- xAxis scales (nonEmpty xs) =<<
         (parent' # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap interior).height } ])
      y <- yAxis scales 3.0 =<<
         (parent' # create G [ classes [ "y-axis" ] ])
      pure { x, y }

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

createStack :: PartialAttrs { x :: String, y :: Number, height :: Number } -> D3.Selection -> StackedBar -> Effect Unit
createStack attrs parent' stackedBar@(StackedBar { i }) = do
   stack <- parent' # create G []
   let bars = barData stackedBar
   for_ bars \bar -> do
      void $ createBar
         ( const $ attrs { x: bar.x, y: bar.y, height: bar.height }
         )
         stack
         bar
   where
   barData :: StackedBar -> NonEmptyArray { i :: Int, j :: Int, x :: String, y :: Number, height :: Number }
   barData (StackedBar { x, bars }) =
      foldl go first tail
      where
      { head: Bar { z, j }, tail } = A.uncons (nonEmpty bars)
      first = A.singleton { i, j, x: xv, y: 0.0, height: fst z }
      xv = fst x

      go acc (Bar { z, j }) =
         A.snoc acc { i, j, x: xv, y: prev.y + prev.height, height: fst z }
         where
         prev = A.last acc

createBar :: PartialAttrs Unit -> D3.Selection -> { i :: Int, j :: Int, x :: String, y :: Number, height :: Number } -> Effect D3.Selection
createBar attrs parent' bar = do
   parent'
      # create Rect
           ( attrs unit <>
                [ classes [ "bar" ]
                ]
           )
      >>= setDatum { i: bar.i, j: bar.j }

setSelStates' :: BarChart -> Select -> D3.Selection -> Effect Unit
setSelStates' (BarChart { stackedBars }) redraw parent = do
   segments <- parent # selectAll ".bar"
   listener <- eventListener (redraw <<< uncurry barChartSegment <<< selectionEventData')

   for_ segments \segment -> do
      segment' <- datum segment
      segment # setAttrs (barAttrs segment') >>= registerMouseListeners listener
   where
   barAttrs :: BarSegmentCoordinate -> Attrs
   barAttrs { i, j } =
      [ "fill" ↦
           ( case persistent of
                None -> col'
                Secondary -> "url(#diagonalHatch-" <> show j <> ")"
                Primary -> colorShade col' (-40)
           )
      , "stroke-width" ↦ "1"
      , "stroke-dasharray" ↦ case transient of
           None -> "none"
           Secondary -> "0.5 1" -- "1 2"
           Primary -> "0.5 1" -- "2 2"
      , "stroke-linecap" ↦ "round"
      , "stroke" ↦
           if persistent /= None || transient /= None then colorShade col' (-70)
           else col'
      ]
      where
      StackedBar { bars } = stackedBars ! i
      Bar { z } = bars ! j
      t = snd z
      persistent = getPersistent t
      transient = getTransient t
      col' = indexCol j

   barChartSegment :: ViewSelSetter BarSegmentCoordinate
   barChartSegment { i, j } = barSegment i j >>> barChart

indexCol :: Int -> String
indexCol = colorScale "schemeAccent"

instance Drawable BarChart where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }

derive instance Newtype StackedBar _
