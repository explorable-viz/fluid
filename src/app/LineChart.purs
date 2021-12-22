module App.LineChart where

import Prelude hiding (absurd)

import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import DataType (cLinePlot)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, class Reflect, Renderer, from, get, get_intOrNumber, get_prim, record)
import Bindings (Bind)
import Lattice (Slice, 𝔹, expand)
import Util (type (×), (×), absurd, fromJust)
import Util.SnocList (SnocList)
import Val (Val(..)) as V
import Val (Val)

newtype LineChart = LineChart { caption :: String × 𝔹, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × 𝔹, data_ :: Array Point }
newtype Point = Point { x :: Number × 𝔹, y :: Number × 𝔹 }

foreign import drawLineChart :: Renderer LineChart

instance reflectPoint :: Reflect (SnocList (Bind (Val Boolean))) Point where
   from r = Point {
      x: get_intOrNumber "x" r,
      y: get_intOrNumber "y" r
   }

instance reflectLinePlot :: Reflect (SnocList (Bind (Val Boolean))) LinePlot where
   from r = LinePlot {
      name: get_prim "name" r,
      data_: record from <$> from (get "data" r)
   }

instance reflectLineChart :: Reflect (SnocList (Bind (Val Boolean))) LineChart where
   from r = LineChart {
      caption: get_prim "caption" r,
      plots: from <$> (from (get "plots" r) :: Array (Slice (Val 𝔹))) :: Array LinePlot
   }

instance reflectLinePlot' :: Reflect (Val Boolean) LinePlot where
   from (v × V.Constr _ c (v1 : Nil)) | c == cLinePlot =
      case expand v (V.Constr false cLinePlot (V.Hole false : Nil)) of
         V.Constr _ _ (u1 : Nil) -> record from (u1 × v1)

lineChartHandler :: Handler
lineChartHandler = const fst

-- (unsafe) the datum associated with a line chart mouse event.
unsafePoint :: Maybe EventTarget -> Point
unsafePoint tgt_opt =
   let tgt = fromJust absurd $ tgt_opt
   in (unsafeCoerce tgt).__data__
