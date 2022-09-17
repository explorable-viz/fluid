module App.BarChart where

import Prelude hiding (absurd)
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (
   Handler, class Reflect, Renderer, Selector,
   from, get_intOrNumber, get_prim, record, selectNth, toggleConstrArg, toggleField
)
import DataType (cBarChart, f_caption, f_data, f_x, f_y)
import Dict (Dict, get)
import Lattice (𝔹, neg)
import Util (type (×), (!), definitely')
import Val (Val)

newtype BarChart = BarChart { caption :: String × 𝔹, data :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val Boolean)) BarChartRecord where
   from r = BarChartRecord {
      x: get_prim f_x r,
      y: get_intOrNumber f_y r
   }

instance Reflect (Dict (Val Boolean)) BarChart where
   from r = BarChart {
      caption: get_prim f_caption r,
      data: record from <$> from (get f_data r)
   }

barChartHandler :: Handler
barChartHandler ev = toggleBar $ unsafeBarIndex $ target ev
   where
   toggleBar :: Int -> Selector
   toggleBar i =
      toggleConstrArg cBarChart 0 $
      toggleField f_data $
      selectNth i $
      neg

   -- [Unsafe] Datum associated with bar chart mouse event; 0-based index of selected bar.
   unsafeBarIndex :: Maybe EventTarget -> Int
   unsafeBarIndex tgt_opt =
      let tgt = definitely' $ tgt_opt
      in (unsafeCoerce tgt).__data__!0
