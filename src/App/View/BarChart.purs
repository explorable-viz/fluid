module App.View.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Sel, from, get_intOrNumber, record, unsafeEventData)
import App.Util.Selector (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cBarChart, f_caption, f_data, f_x, f_y)
import Dict (Dict, get)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Util (type (×), (!))
import Val (Val)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

newtype BarChart = BarChart { caption :: String × Sel, data :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × Sel, y :: Number × Sel }

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val Sel)) BarChartRecord where
   from r = BarChartRecord
      { x: unpack string (get f_x r)
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val Sel)) BarChart where
   from r = BarChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

barChartHandler :: Handler
barChartHandler = toggleBar <<< barIndex <<< target
   where
   toggleBar :: Int -> Selector Val
   toggleBar i =
      constrArg cBarChart 0
         $ field f_data
         $ listElement i
         $ neg

   -- [Unsafe] 0-based index of selected bar.
   barIndex :: Maybe EventTarget -> Int
   barIndex tgt_opt = unsafeEventData tgt_opt ! 0
