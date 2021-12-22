module App.BarChart where

import Prelude hiding (absurd)
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, class Reflect, Renderer, from, get, get_intOrNumber, get_prim, record)
import Bindings (Bind)
import Lattice (𝔹)
import Util (type (×), absurd, fromJust)
import Util.SnocList (SnocList)
import Val (Val)

newtype BarChart = BarChart { caption :: String × 𝔹, data_ :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: Renderer BarChart

instance reflectBarChartRecord :: Reflect (SnocList (Bind (Val Boolean))) BarChartRecord where
   from r = BarChartRecord {
      x: get_prim "x" r,
      y: get_intOrNumber "y" r
   }

instance reflectBarChart :: Reflect (SnocList (Bind (Val Boolean))) BarChart where
   from r = BarChart {
      caption: get_prim "caption" r,
      data_: record from <$> from (get "data" r)
   }

barChartHandler :: Handler
barChartHandler = const fst

-- (unsafe) datum associated with bar chart mouse event; the 0-based index of the selected bar
unsafeBarChartRecord :: Maybe EventTarget -> Int
unsafeBarChartRecord tgt_opt =
   let tgt = fromJust absurd $ tgt_opt
   in (unsafeCoerce tgt).__data__[0]
