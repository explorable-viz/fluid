module App.LineChart where

import Prelude hiding (absurd)

import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (
   Handler, class Reflect, Renderer, Selector,
   from, get, get_intOrNumber, get_prim, record, toggleConstrArg, toggleField, toggleNth
)
import Bindings (Bind)
import DataType (cLineChart, cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Lattice (Slice, 𝔹, expand, neg)
import Util (type (×), (×), (!), definitely')
import Util.SnocList (SnocList)
import Val (Val(..))

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
      plots: from <$> (from (get f_plots r) :: Array (Slice (Val 𝔹))) :: Array LinePlot
   }

instance reflectLinePlot' :: Reflect (Val Boolean) LinePlot where
   from (v × Constr _ c (v1 : Nil)) | c == cLinePlot =
      case expand v (Constr false c (Hole false : Nil)) of
         Constr _ _ (u1 : Nil) -> record from (u1 × v1)

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
      fst >>> neg

   -- [Unsafe] Datum associated with line-chart mouse event; 0-based indices of line plot and point
   -- within line plot.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt =
      let tgt = definitely' $ tgt_opt
          xy = (unsafeCoerce tgt).__data__!0 :: Array Int
      in xy!0 × xy!1
