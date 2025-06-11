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
import App.View.Util.D3 (ElementType(..), Margin, create, setText, translate)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Array (mapWithIndex, uncons)
import Data.Foldable (for_)
import Data.Int (floor, pow, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (log)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Util (absurd, error, (!))
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

foreign import createRootElement2 :: BarChartHelpers -> UIHelpers -> BarChart -> D3.Selection -> Effect D3.Selection
foreign import setSelStates2 :: BarChartHelpers -> BarChart -> Select -> D3.Selection -> Effect Unit

createRootElement' :: BarChart -> D3.Selection -> Effect D3.Selection
createRootElement' barchart@(BarChart { caption, size, stackedBars }) parent = do
   rootElement <- createRootElement2 barChartHelpers uiHelpers barchart parent
   let
      interior :: Dimensions Int
      interior = Dimensions
         { width: width - margin.left - margin.right
         , height: height - margin.top - margin.bottom
         }
   createLegend interior rootElement

   rootElement
      # create Text
           [ "x" ⟼ width / 2
           , "y" ⟼ height + 35
           , classes [ caption_class ]
           , "dominant-baseline" ↦ "central"
           , "text-anchor" ↦ "middle"
           ]
      >>= setText (fst caption)

   where
   names = case (uncons stackedBars) of
      Nothing -> error absurd
      Just { head: StackedBar bar, tail: _ } -> (\(Bar bar') -> fst bar'.y) <$> bar.bars

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
   legendStart = width + margin.left / 2

   createLegend :: Dimensions Int -> D3.Selection -> Effect Unit
   createLegend (Dimensions interior) parent' = do
      let { height, width } = interior
      legend' <- parent' # create G [ translate { x: legendStart, y: height / 2 - margin.top - 2 } ]
      void $ legend' # create Rect
         [ classes [ "legend-box" ], "x" ⟼ 0, "y" ⟼ 0, "height" ⟼ height, "width" ⟼ width ]
      for_ entries \{ i, name } -> do
         g <- legend' # create G [ classes [ "legend-entry" ], translate { x: 0, y: entry_y i } ]
         void $ g #
            ( create Text [ classes [ "legend-text" ], translate { x: legend_entry_x, y: 9 } ]
                 >=> setText name
            )
      where
      entries :: Array LegendEntry
      entries = flip mapWithIndex names \i name -> { i, name }
      entry_y i = i * legendLineHeight + 2
   legend_entry_x = 15

barChartHelpers :: BarChartHelpers
barChartHelpers =
   { bar_attrs
   , tickEvery
   , withBarChartSegment
   }
   where
   bar_attrs :: (Int -> String) -> BarChart -> BarSegmentCoordinate -> Object String
   bar_attrs indexCol (BarChart { stackedBars }) { i, j } =
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
      col = indexCol j

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

instance Drawable BarChart where
   createRootElement = createRootElement'
   setSelStates = setSelStates2 barChartHelpers

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }
