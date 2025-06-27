module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), Selectable, Attrs, classes, contents)
import App.Util.Selector (barChart, dictVal, listElement)
import App.View.Segment (Segment(..), Scales, indexCol)
import App.View.StackedBar (StackedBar(..), StackedBarContext, barHeight)
import App.View.Util (class View, class View2, Select, createElement, createRootElement, setSelStates, setSelection)
import App.View.Util.D3 (Coord, ElementType(..), Margin, addHatchPattern, bandwidth, create, scaleBand, scaleLinear, selectAll, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (range)
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Array.NonEmpty (head) as A
import Data.Foldable (for_, length, sum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Number (ceil)
import Data.Semigroup.Foldable (maximum)
import DataType (f_stackedBars)
import Effect (Effect)
import Util (Endo, (!))

newtype BarChart = BarChart
   { caption :: Selectable String
   , size :: Dimensions (Selectable Int)
   , stackedBars :: NonEmptyArray StackedBar
   }

setSelStates' :: BarChart -> Select -> D3.Selection -> Effect Unit
setSelStates' (BarChart { stackedBars }) select barChart' = do
   -- more robust to iterate over stackedBars and select ith DOM child instead?
   stackedBars' <- barChart' # selectAll ".stack"
   forWithIndex_ stackedBars' \i stack ->
      setSelStates (stackedBars ! i) (select <<< barChart <<< dictVal f_stackedBars <<< listElement i) stack

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' (BarChart { caption, size, stackedBars }) parent = do
   svg <- parent # create SVG [ "width" ⟼ width, "height" ⟼ height ]

   g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
   void $ createAxes g
   createStackedBars g

   for_ (range 0 $ length ys - 1) \j ->
      addHatchPattern g j $ indexCol j

   void $ svg
      # create Text
           [ "x" ⟼ width / 2
           , "y" ⟼ height - caption_height / 2
           , classes [ caption_class ]
           , "dominant-baseline" ↦ "central"
           , "text-anchor" ↦ "middle"
           ]
      >>= setText (contents caption)

   createLegend interior g
   pure g
   where
   -- assume all bars have same set of y indices
   ys = bar.segments <#> \(Segment seg) -> contents seg.y
      where
      StackedBar bar = A.head stackedBars

   xs = stackedBars <#> \(StackedBar bar) -> contents bar.x

   Dimensions { width, height } = size <#> contents

   margin :: Margin
   margin = { top: 3, right: 75, bottom: 20, left: 30 }

   interior :: Dimensions Int
   interior = Dimensions
      { width: width - margin.left - margin.right
      , height: height - margin.top - margin.bottom - caption_height
      }

   caption_class = "title-text"
   caption_height = textHeight caption_class (contents caption) * 2

   legend_entry_x = 15
   legendSquareSize = 4
   legendLineHeight = 15

   legend_dims :: Dimensions Int
   legend_dims = Dimensions
      { width: legend_entry_x + maxTextWidth + rightMargin
      , height: legendLineHeight * length ys
      }
      where
      maxTextWidth = maximum (ys <#> textWidth "legend-text")
      rightMargin = 4

   scales = to interior
   nearest = 10.0
   y_max = ceil $ nearest *
      ((maximum $ map (\(StackedBar bar) -> (sum $ map (\(Segment b) -> contents b.z) bar.segments)) stackedBars) / nearest)

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: scaleBand width $ map (\(StackedBar r) -> contents r.x) (toArray stackedBars)
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   createStackedBars :: D3.Selection -> Effect Unit
   createStackedBars parent' =
      for_ stackedBars \stackedBar ->
         createRootElement attrFun stackedBar parent'
      where
      attrFun :: { x :: String, y :: Number } -> Attrs -> { z :: Number } -> Attrs
      attrFun seg attrs seg' =
         [ "x" ⟼ scales.x seg.x
         , "y" ⟼ scales.y (seg'.z + seg.y)
         , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth) - scales.y seg'.z
         , "stroke-width" ⟼ strokeWidth
         , "width" ⟼ bandwidth scales.x
         ] <> attrs

      strokeWidth = 1

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior') parent' = do
      let Dimensions { height, width } = legend_dims
      legend' <- parent' # create G
         [ translate { x: interior'.width + 30, y: max 0 $ (interior'.height - height) / 2 } ]
      void $ legend' # create Rect
         [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      forWithIndex_ ys \i y -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText y
            )
         g # create Rect
            [ "fill" ↦ indexCol i
            , "width" ⟼ legendSquareSize
            , "height" ⟼ legendSquareSize
            , "x" ⟼ legendLineHeight / 2 - legendSquareSize / 2
            , "y" ⟼ legendLineHeight / 2 - legendSquareSize
            ]
      where
      entry_y i = i * legendLineHeight + 2

   createAxes :: D3.Selection -> Effect (Coord D3.Selection)
   createAxes parent' = do
      x <- xAxis scales xs =<<
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
      void $ pattern # create Line
         [ "x1" ⟼ 0
         , "y" ⟼ 0
         , "x2" ⟼ 0
         , "y2" ⟼ 3.5
         , "stroke" ↦ "rgba(255, 255, 255, 1)"
         , "stroke-width" ↦ "1"
         ]

instance View BarChart Unit { x :: String, y :: Number } where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

instance View2 BarChart Unit where
   setSelection :: Unit -> BarChart -> Select -> D3.Selection -> Effect Unit
   setSelection _ chart@(BarChart { stackedBars }) select barChart' = do
      let props = barChartProps chart
      -- more robust to iterate over stackedBars and select ith DOM child instead?
      stackedBars' <- barChart' # selectAll ".stack"
      forWithIndex_ stackedBars' \i stack ->
         setSelection props.stackedBarContext (stackedBars ! i)
            (select <<< barChart <<< dictVal f_stackedBars <<< listElement i)
            stack

   createElement :: Unit -> BarChart -> D3.Selection -> Effect D3.Selection
   createElement _ barChart@(BarChart { caption, stackedBars }) parent = do
      svg <- parent # create SVG [ "width" ⟼ props.width, "height" ⟼ props.height ]
      g <- svg # create G [ translate { x: props.margin.left, y: props.margin.top } ]
      void $ createAxes g
      createStackedBars g

      for_ (range 0 $ length props.ys - 1) \y_index ->
         addHatchPattern g y_index $ indexCol y_index

      void $ svg
         # create Text
              [ "x" ⟼ props.width / 2
              , "y" ⟼ props.height - props.caption_height / 2
              , classes [ props.caption_class ]
              , "dominant-baseline" ↦ "central"
              , "text-anchor" ↦ "middle"
              ]
         >>= setText (contents caption)

      createLegend props.interior g
      pure g

      where
      props = barChartProps barChart

      createAxes :: D3.Selection -> Effect (Coord D3.Selection)
      createAxes parent' = do
         x <- xAxis props.scales props.xs =<<
            (parent' # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap props.interior).height } ])
         y <- yAxis props.scales 3.0 =<<
            (parent' # create G [ classes [ "y-axis" ] ])
         pure { x, y }

      createStackedBars :: D3.Selection -> Effect Unit
      createStackedBars parent' =
         for_ stackedBars \stackedBar ->
            createElement props.stackedBarContext stackedBar parent'

      createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
      createLegend (Dimensions interior') parent' = do
         legend' <- parent' # create G
            [ translate { x: interior'.width + 30, y: max 0 $ (interior'.height - height) / 2 } ]
         void $ legend' # create Rect
            [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
         forWithIndex_ props.ys \y_index y -> do
            g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y y_index } ]
            void $ g #
               ( create Text [ classes [ "legend-text" ], translate { x: entry_x, y: 9 } ]
                    >=> setText y
               )
            g # create Rect
               [ "fill" ↦ indexCol y_index
               , "width" ⟼ squareSize
               , "height" ⟼ squareSize
               , "x" ⟼ lineHeight / 2 - squareSize / 2
               , "y" ⟼ lineHeight / 2 - squareSize
               ]
         where
         height = lineHeight * length props.ys
         width = entry_x + maxTextWidth + rightMargin
         lineHeight = 15
         entry_x = 15
         squareSize = 4
         rightMargin = 4
         maxTextWidth = maximum (props.ys <#> textWidth "legend-text")
         entry_y i = i * lineHeight + 2

type BarChartProperties =
   { width :: Int
   , height :: Int
   , xs :: NonEmptyArray String
   , ys :: NonEmptyArray String
   , margin :: Margin
   , interior :: Dimensions Int
   , scales :: Scales
   , caption_class :: String
   , caption_height :: Int
   , stackedBarContext :: StackedBarContext
   }

barChartProps :: BarChart -> BarChartProperties
barChartProps (BarChart { caption, size, stackedBars }) =
   { width
   , height
   , xs
   , ys
   , margin
   , interior
   , scales
   , caption_height
   , caption_class
   , stackedBarContext: { interior, scales, strokeWidth }
   }
   where

   xs = stackedBars <#> \(StackedBar bar) -> contents bar.x
   ys = (unwrap $ head stackedBars).segments <#> \(Segment seg) -> contents seg.y -- TODO: check uniformity for each bar
   Dimensions { width, height } = size <#> contents

   margin :: Margin
   margin = { top: 3, right: 75, bottom: 20, left: 30 }

   interior :: Dimensions Int
   interior = Dimensions
      { width: width - margin.left - margin.right
      , height: height - margin.top - margin.bottom - caption_height
      }

   scales = to interior

   caption_class = "title-text"
   caption_height = textHeight caption_class (contents caption) * 2

   to :: Dimensions Int -> Scales
   to (Dimensions { width, height }) =
      { x: scaleBand width $ (\(StackedBar bar) -> contents bar.x) <$> toArray stackedBars
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   nearest = 10.0
   y_max = ceil $ nearest * (maximum (barHeight <$> stackedBars) / nearest)
   strokeWidth = 1
