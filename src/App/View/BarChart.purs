module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, SelState, Selectable, Selector, 𝕊, from, get_intOrNumber, record, selector, unsafeEventData)
import App.Util.Selector (barChart, barSegment)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import DataType (f_bars, f_caption, f_data, f_x, f_y, f_z)
import Dict (Dict)
import Primitive (string, unpack)
import Util (type (×), Endo, (×))
import Util.Map (get)
import Val (Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.EventTarget (EventTarget)

newtype BarChart = BarChart
   { caption :: Selectable String
   , data :: Array StackedBar
   }

newtype StackedBar = StackedBar
   { x :: Selectable String
   , bars :: Array Bar
   }

newtype Bar = Bar
   { y :: Selectable String
   , z :: Selectable Number
   }

type BarChart' =
   { chart :: BarChart
   , selData :: BarChartSelState
   }

type BarChartSelState = Array (Array (SelState 𝕊))

-- Selection state actually used in UI
barChartSelState :: BarChart -> BarChartSelState
barChartSelState (BarChart r) =
   r.data <#> \(StackedBar { bars }) -> bars <#> \(Bar { z: _ × α }) -> α

foreign import drawBarChart :: Renderer BarChart'

instance Reflect (Dict (Val (SelState 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

instance Reflect (Dict (Val (SelState 𝕊))) StackedBar where
   from r = StackedBar
      { x: unpack string (get f_x r)
      , bars: record from <$> from (get f_bars r)
      }

instance Reflect (Dict (Val (SelState 𝕊))) Bar where
   from r = Bar
      { y: unpack string (get f_y r)
      , z: get_intOrNumber f_z r
      }

-- see data binding in BarChart.js
type BarSegmentCoordinate = { i :: Int, j :: Int }

barChartHandler :: Handler
barChartHandler = (target &&& type_) >>> barSegmentCoord >>> uncurry toggleSegment
   where
   toggleSegment :: BarSegmentCoordinate -> Endo (Selector Val)
   toggleSegment { i, j } = barSegment i j >>> barChart

   barSegmentCoord :: Maybe EventTarget × EventType -> BarSegmentCoordinate × Selector Val
   barSegmentCoord (tgt_opt × ty) = unsafeEventData tgt_opt × selector ty
