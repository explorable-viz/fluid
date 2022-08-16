module App.LineChart2 where

import Prelude hiding (absurd)

import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util2 (
   Handler, class Reflect, Renderer, Selector,
   from, get, get_intOrNumber, get_prim, record, toggleConstrArg, toggleField, toggleNth
)
import Bindings2 (Bind)
import DataType2 (cLineChart, cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Lattice2 (𝔹, neg)
import Util2 (type (×), (×), (!), definitely')
import Util.SnocList2 (SnocList)
import Val2 (Val(..))

newtype LineChart = LineChart { caption :: String × 𝔹, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × 𝔹, data :: Array Point }
newtype Point = Point { x :: Number × 𝔹, y :: Number × 𝔹 }

foreign import drawLineChart :: Renderer LineChart

instance reflectPoint :: Reflect (SnocList (Bind (Val Boolean))) Point where
   from r = Point {
      x: get_intOrNumber f_x r,
      y: get_intOrNumber f_y r
   }

instance reflectLinePlot :: Reflect (SnocList (Bind (Val Boolean))) LinePlot where
   from r = LinePlot {
      name: get_prim f_name r,
      data: record from <$> from (get f_data r)
   }

instance reflectLineChart :: Reflect (SnocList (Bind (Val Boolean))) LineChart where
   from r = LineChart {
      caption: get_prim f_caption r,
      plots: from <$> (from (get f_plots r) :: Array (Val 𝔹)) :: Array LinePlot
   }

instance reflectLinePlot' :: Reflect (Val Boolean) LinePlot where
   from (Constr _ c (u1 : Nil)) | c == cLinePlot = record from u1

lineChartHandler :: Handler
lineChartHandler ev = togglePoint $ unsafePos $ target ev
   where
   togglePoint :: Int × Int -> Selector
   togglePoint (i × j) =
      toggleConstrArg cLineChart 0 $
      toggleField f_plots $
      toggleNth i $
      toggleConstrArg cLinePlot 0 $
      toggleField f_data $
      toggleNth j $
      neg

   -- [Unsafe] Datum associated with line-chart mouse event; 0-based indices of line plot and point
   -- within line plot.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt =
      let tgt = definitely' $ tgt_opt
          xy = (unsafeCoerce tgt).__data__!0 :: Array Int
      in xy!0 × xy!1
