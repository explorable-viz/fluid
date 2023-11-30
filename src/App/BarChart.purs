module App.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, from, get_intOrNumber, record)
import App.Util.Select (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cBarChart, f_caption, f_data, f_x, f_y)
import Dict (Dict, get)
import Lattice (𝔹, neg)
import Primitive (string2, unpack2)
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (!), definitely')
import Val (Val)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

newtype BarChart = BarChart { caption :: String × 𝔹, data :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val 𝔹)) BarChartRecord where
   from r = BarChartRecord
      { x: unpack2 string2 (get f_x r)
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val 𝔹)) BarChart where
   from r = BarChart
      { caption: unpack2 string2 (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

barChartHandler :: Handler
barChartHandler ev = toggleBar $ unsafeBarIndex $ target ev
   where
   toggleBar :: Int -> Selector Val
   toggleBar i =
      constrArg cBarChart 0
         $ field f_data
         $ listElement i
         $ neg

   -- [Unsafe] Datum associated with bar chart mouse event; 0-based index of selected bar.
   unsafeBarIndex :: Maybe EventTarget -> Int
   unsafeBarIndex tgt_opt = (unsafeCoerce $ definitely' tgt_opt).__data__ ! 0
