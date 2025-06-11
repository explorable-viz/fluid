module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (Selectable, 𝕊(..), classes, colorShade, getPersistent, getTransient, selectionEventData')
import App.Util.Selector (ViewSelSetter, barChart, barSegment)
import App.View.Util (class Drawable, UIHelpers, Select, uiHelpers)
import App.View.Util.D3 (ElementType(..), create, setText)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Int (floor, pow, toNumber)
import Data.Number (log)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Util ((!))
import Web.Event.EventTarget (EventListener, eventListener)

newtype BarChart = BarChart
   { caption :: Selectable String
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
   createRootElement barchart@(BarChart { caption }) parent = do
      rootElement <- createRootElement2 barChartHelpers uiHelpers barchart parent
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
      caption_class = "title-text"
      width = 160
      height = 127
   setSelStates = setSelStates2 barChartHelpers

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }
