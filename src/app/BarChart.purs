module App.BarChart where

import Prelude hiding (absurd)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, class Reflect, Renderer, from, get, get_intOrNumber, get_prim, record)
import Bindings (Bind)
import DataType (cBarChart)
import Lattice (𝔹, expand)
import Util (type (×), (×), absurd, error, fromJust)
import Util.SnocList (SnocList)
import Val (Val(..))

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
barChartHandler ev (u × Constr _ c (v1 : Nil)) | c == cBarChart =
   case expand u (Constr false cBarChart (Hole false : Nil)) of
      Constr α _ (u1 : Nil) ->
         let i = unsafeBarChartRecord (target ev) in
         Constr α cBarChart (u1 : Nil)
      _ -> error absurd
   where
   -- (unsafe) datum associated with bar chart mouse event; 0-based index of selected bar
   unsafeBarChartRecord :: Maybe EventTarget -> Int
   unsafeBarChartRecord tgt_opt =
      let tgt = fromJust absurd $ tgt_opt
      in (unsafeCoerce tgt).__data__[0]
barChartHandler _ _ = error absurd
