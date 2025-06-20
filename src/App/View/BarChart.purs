module App.View.BarChart
   ( BarChart(..)
   ) where

import Prelude hiding (absurd)

import App.Segment (Segment(..), indexCol)
import App.Util (Dimensions(..), Selectable, classes)
import App.Util.Selector (barChart, dictVal)
import App.View.LineChart (LegendEntry)
import App.View.StackedBar (StackedBar(..))
import App.View.Util (class Drawable, Select, createRootElement, setSelStates)
import App.View.Util.D3 (Coord, ElementType(..), Margin, bandwidth, create, datum, scaleBand, scaleLinear, selectAll, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (elemIndex, length, mapWithIndex, range)
import Data.Array.NonEmpty (head) as A
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Number (ceil)
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst)
import DataType (f_stackedBars)
import Effect (Effect)
import Util (Endo, definitely', nonEmpty, (!))

newtype BarChart = BarChart
   { caption :: Selectable String
   , size :: Dimensions (Selectable Int)
   , stackedBars :: (Array StackedBar)
   }

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' (BarChart { caption, size, stackedBars }) parent = do
   svg <- parent # create SVG [ "width" ⟼ width, "height" ⟼ height ]

   g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
   void $ createAxes g
   createStacks g strokeWidth

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
   ys = bar.bars <#> \(Segment bar') -> fst bar'.y
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
   y_max = ceil $ ((maximum $ map (\(StackedBar bar) -> (sum $ map (\(Segment b) -> fst b.z) bar.bars)) stackedBars') / nearest) * nearest

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: scaleBand width fst $ map (\(StackedBar r) -> { x: r.x }) stackedBars
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }
   strokeWidth = 1
   barChartAttrs =
      [ "stroke-width" ⟼ strokeWidth
      , "width" ⟼ bandwidth scales.x
      ]

   createStacks :: D3.Selection -> Int -> Effect Unit
   createStacks parent' strokeWidth' = do
      for_ stackedBars \stackedBar -> do
         createRootElement attrFun stackedBar parent'
      where
      attrFun bar attrs segment =
         [ "x" ⟼ scales.x bar.x
         , "y" ⟼ scales.y (segment.z + bar.y)
         , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth') - scales.y segment.z
         ] <> barChartAttrs <> attrs

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

setSelStatesBarChart :: BarChart -> Select -> D3.Selection -> Effect Unit
setSelStatesBarChart (BarChart { stackedBars }) select parent = do
   stacks <- parent # selectAll ".stack"
   for_ stacks \stack -> do
      { i } <- datum stack
      setSelStates (stackedBars ! i) (select <<< barChart <<< dictVal f_stackedBars) stack

instance Drawable BarChart Unit { x :: String, y :: Number, height :: Number } where
   createRootElement _ = createRootElement'
   setSelStates = setSelStatesBarChart

