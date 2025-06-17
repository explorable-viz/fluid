module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), Selectable, 𝕊(..), Attrs, classes, colorShade, getPersistent, getTransient, selectionEventData')
import App.Util.Selector (ViewSelSetter, barChart, barSegment)
import App.View.LineChart (LegendEntry)
import App.View.Util (class Drawable, Select, registerMouseListeners)
import App.View.Util.D3 (Coord, ElementType(..), Margin, bandwidth, colorScale, colorScale2, create, datum, scaleBand, scaleLinear, selectAll, setAttrs, setDatum, setText, textHeight, textWidth, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (length, mapWithIndex, range, uncons)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (last, singleton, snoc, uncons) as A
import Data.Foldable (for_, sum)
import Data.FoldableWithIndex (foldlWithIndex, forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number (ceil)
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object)
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

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' (BarChart { caption, size, stackedBars }) parent = do
   svg <- parent # create SVG [ "width" ⟼ width, "height" ⟼ height ]

   g <- svg # create G [ translate { x: margin.left, y: margin.top } ]
   void $ createAxes g
   createStacks g 1

   for_ js \j -> do
      addHatchPattern g j $ col j js
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
   names = case (uncons stackedBars) of
      Nothing -> error absurd
      Just { head: StackedBar bar, tail: _ } -> (\(Bar bar') -> fst bar'.y) <$> bar.bars
<<<<<<< HEAD
   xs = stackedBars <#> \(StackedBar bar) -> fst bar.x
=======
   xs = (\(StackedBar bar) -> fst $ bar.x) <$> stackedBars
>>>>>>> 41495d58cda469f441182e7f22855997a2515032
   js = range 0 j_max
      where
      j_max = maximum (map (\(StackedBar bar) -> length bar.bars - 1) (nonEmpty stackedBars))

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

   createStacks :: D3.Selection -> Int -> Effect Unit
   createStacks parent' strokeWidth = do
      forWithIndex_ stackedBars \i stackedBar -> do
         stack <- parent' # create G []
         let bars = barData i stackedBar
         forWithIndex_ bars \j bar -> do
            void $ stack #
               ( create Rect
                    [ classes [ "bar" ]
                    , "x" ⟼ scales.x bar.x
                    , "y" ⟼ scales.y (bar.y + bar.height)
                    , "width" ⟼ bandwidth scales.x
                    , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth) - scales.y bar.height
                    , "stroke-width" ⟼ strokeWidth
                    ] >=> setDatum { i, j }
               )
      where
      barData :: Int -> StackedBar -> NonEmptyArray { i :: Int, j :: Int, x :: String, y :: Number, height :: Number }
      barData i (StackedBar { x, bars }) =
         let
            { head: Bar { z }, tail } = A.uncons (nonEmpty bars)
            first = A.singleton { i, j: 0, x: xv, y: 0.0, height: fst z }
         in
            foldlWithIndex go first tail
         where
         xv = fst x

         go j acc (Bar { z }) =
            let
               prev = A.last acc
            in
               A.snoc acc { i, j, x: xv, y: prev.y + prev.height, height: fst z }

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior') parent' = do
      let Dimensions { height, width } = legend_dims
      legend' <- parent' # create G
         [ translate { x: interior'.width + 30, y: max 0 ((interior'.height - height) / 2) } ]
      void $ legend' # create Rect
         [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      for_ entries \{ i, name } -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText name
            )
         g # create Rect
            [ "fill" ↦ col name names
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

   createAxes :: D3.Selection -> Effect (Coord D3.Selection)
   createAxes parent' = do
      x <- xAxis scales (nonEmpty xs) =<<
         (parent' # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap interior).height } ])
      y <- yAxis scales 3.0 =<<
         (parent' # create G [ classes [ "y-axis" ] ])
      pure { x, y }

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: (scaleBand width fst (map ((\(StackedBar r) -> { x: r.x })) stackedBars))
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   interior :: Dimensions Int
   interior = Dimensions
      { width: width - margin.left - margin.right
      , height: height - margin.top - margin.bottom - caption_height
      }

   scales = (to interior)

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

col :: forall a. a -> Array a -> String
col = colorScale "schemeAccent"

indexCol :: Int -> String
indexCol = colorScale2 "schemeAccent"

instance Drawable BarChart where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }

derive instance Newtype StackedBar _
