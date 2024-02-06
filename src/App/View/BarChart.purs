module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Sel, Selectable, from, get_intOrNumber, record, unsafeEventData)
import App.Util.Selector (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cBarChart, f_bars, f_caption, f_data, f_x, f_y, f_z)
import Dict (Dict)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Util (spy)
import Util.Map (get)
import Val (Val)
import Web.Event.Event (target)
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

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val Sel)) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

instance Reflect (Dict (Val Sel)) StackedBar where
   from r = StackedBar
      { x: unpack string (get f_x r)
      , bars: record from <$> from (get f_bars r)
      }

instance Reflect (Dict (Val Sel)) Bar where
   from r = Bar
      { y: unpack string (get f_y r)
      , z: get_intOrNumber f_z r
      }

-- see data binding in BarChart.js
type BarSegmentCoordinate = { i :: Int, j :: Int }

barChartHandler :: Handler
barChartHandler = target >>> barSegment >>> toggleBar
   where
   toggleBar :: BarSegmentCoordinate -> Selector Val
   toggleBar { i, j } =
      constrArg cBarChart 0
         $ field f_data
         $ listElement i
         $ field f_bars
         $ listElement j
         $ field f_z
         $ neg

   barSegment :: Maybe EventTarget -> BarSegmentCoordinate
   barSegment ev =
      let q = unsafeEventData ev
      in spy "q" identity q
