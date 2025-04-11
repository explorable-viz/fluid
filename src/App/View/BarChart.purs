module App.View.BarChart
   ( Bar(..)
   , BarChart(..)
   , StackedBar(..)
   ) where

import Prelude hiding (absurd)

import App.Util (class Reflect, SelStates, Selectable, 𝕊(..), colorShade, dict, from, getPersistent, getTransient, get_intOrNumber)
import App.Util.Selector (ViewSelSetter, barChart, barSegment)
import App.View.Util (class Drawable, class Drawable2, Renderer, selListener', uiHelpers)
import App.View.Util.D3 (ElementType(..), create)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Int (floor, pow, toNumber)
import Data.Number (log)
import Data.Tuple (snd)
import DataType (f_bars, f_caption, f_stackedBars, f_x, f_y, f_z)
import Dict (Dict)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Primitive (string, unpack)
import Util (type (×), (!))
import Util.Map (get)
import Val (Val)
import Web.Event.EventTarget (EventListener)

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
   }

foreign import drawBarChart :: BarChartHelpers -> Renderer BarChart

barChartHelpers :: BarChartHelpers
barChartHelpers =
   { bar_attrs
   , tickEvery
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

setSelStates2 :: BarChart -> EventListener -> D3.Selection -> Effect Unit
setSelStates2 _ _ _ =
   pure unit

createRootElement2 :: BarChart -> D3.Selection -> String -> Effect D3.Selection
createRootElement2 _ div _ = do
   rootElement <- div # create SVG []
   pure rootElement

instance Drawable2 BarChart where
   createRootElement = createRootElement2
   setSelStates = setSelStates2

instance Drawable BarChart where
   draw rSpec figVal _ redraw =
      drawBarChart barChartHelpers uiHelpers rSpec =<< selListener' figVal redraw barSegment'
      where
      barSegment' :: ViewSelSetter BarSegmentCoordinate
      barSegment' { i, j } = barSegment i j >>> barChart

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (snd (get f_caption r))
      , stackedBars: dict from <$> from (snd (get f_stackedBars r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) StackedBar where
   from r = StackedBar
      { x: unpack string (snd (get f_x r))
      , bars: dict from <$> from (snd (get f_bars r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) Bar where
   from r = Bar
      { y: unpack string (snd (get f_y r))
      , z: get_intOrNumber f_z r
      }

-- see data binding in .js
type BarSegmentCoordinate = { i :: Int, j :: Int }
