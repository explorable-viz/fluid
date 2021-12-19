module App.BarChart where

import Prelude hiding (absurd)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, target)
import Web.Event.EventTarget (EventListener, EventTarget)
import App.Util (HTMLId, class Reflect, from, get, get_intOrNumber, get_prim, record)
import Bindings (Bind)
import Lattice (𝔹)
import Util (type (×), absurd, fromJust)
import Util.SnocList (SnocList)
import Val (Val)

newtype BarChart = BarChart { caption :: String × 𝔹, data_ :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: HTMLId -> BarChart -> EventListener -> Effect Unit

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

barChartHandler :: Event -> Effect Unit
barChartHandler ev = do
   let BarChartRecord xy = unsafeBarChartRecord (target ev)
   log $ show xy
   pure unit

-- (unsafe) the datum associated with a bar chart mouse event.
unsafeBarChartRecord :: Maybe EventTarget -> BarChartRecord
unsafeBarChartRecord tgt_opt =
   let tgt = fromJust absurd $ tgt_opt
   in (unsafeCoerce tgt).__data__
